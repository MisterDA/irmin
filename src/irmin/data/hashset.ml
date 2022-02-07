(*
 * Copyright (c) 2021-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
include Hashset_intf

module Fixed_size_string = struct
  (** {1 Implementation details}

      We implement the string set as a hashset: an array of buckets, each
      storing the elements that share a common hash prefix. The string data is
      stored in a separate arena (contiguous region of bytes) that is expanded
      whenever it gets too large, with the buckets themselves containing
      {i indices} into this arena. This indirection has two advantages:

      - keeping all of the strings in an arena avoids allocating header words +
        null padding for each of them, saving up to 2 words per element;

      - using arena indices in the hashset allows the buckets to be
        [Immediate_array]s, meaning that buckets of size 0 and 1 are inlined
        directly in the parent hashset, with buckets of two or more elements
        being represented as separately-allocated arrays.

      For typical load factors, inlining singleton buckets into the parent array
      is a considerable memory reduction (~20%) and avoids some unnecessary
      allocations. This comes at the cost of generating more garbage when adding
      elements to large (> 1 element) buckets, since the entire bucket must be
      copied into a new heap block of size [n+1] to store the new element. Since
      the majority of buckets have size 0 or 1, this is a reasonable trade-off
      for certain use-cases.

      Overall, this results in a very compact in-memory representation. For
      example, a hashset of the strings ["a" ..."d"] might be represented as
      follows:

      {v
      arena:

        ┌─────┬─────┬─────┬─────┐
        │ 'a' │ 'b' │ 'c' │ 'd' │
        └─────┴─────┴─────┴─────┘

      hashset:

        ┌─────┐
        │ { } │
        ├─────┤
        │  0  │                            { hash "a"         mod 4 = 1 }
        ├─────┤    ┌─────┬─────┬─────┐
        │  ┄┄┄┼┄┄┄>│ hdr │  3  │  2  │     { hash ("c" | "b") mod 4 = 2 }
        ├─────┤    └─────┴─────┴─────┘
        │  4  │                            { hash "d"         mod 4 = 3 }
        └─────┘
      v} *)

  type bucket = Arena.id Immediate_array.t

  type t = {
    arena : Arena.t;
    hash_elt : string -> int;
    mutable hashset : bucket array;
    mutable bucket_count_log2 : int;
  }

  module Bucket = struct
    include Immediate_array

    let cons : type a. a -> a t -> a t =
     fun x xs ->
      let old_len = length xs in
      let len = old_len + 1 in
      let arr = create ~len x in
      let arr =
        unsafe_blit ~src:xs ~dst:arr ~src_pos:0 ~dst_pos:1 ~len:old_len
      in
      arr
  end

  let arena_capacity ~bucket_count =
    (* Expand the hashset when there are ~2 elements per bucket *)
    2 * bucket_count

  let create ~elt_length ~initial_capacity ~hash:hash_elt () =
    let bucket_count_log2, bucket_count =
      let rec aux n_log2 n =
        if n >= initial_capacity then (n_log2, n)
        else if n * 2 > Sys.max_array_length then (n_log2, n)
        else aux (n_log2 + 1) (n * 2)
      in
      aux 0 1
    in
    let hashset = Array.make bucket_count (Bucket.empty ()) in
    let arena =
      Arena.create ~elt_length ~initial_capacity:(arena_capacity ~bucket_count)
    in
    { hashset; hash_elt; arena; bucket_count_log2 }

  let elt_index t elt = t.hash_elt elt land ((1 lsl t.bucket_count_log2) - 1)

  let mem t elt =
    let bucket = t.hashset.(elt_index t elt) in
    Bucket.exists bucket ~f:(fun id -> Arena.elt_equal t.arena id elt)

  exception Nondeterministic_hash of string

  let raise_nondeterministic_hash fmt =
    Format.kasprintf (fun s -> raise (Nondeterministic_hash s)) fmt

  let resize =
    let partition_bucket (t : t) ~old_index ~new_bucket_count bucket :
        bucket * bucket =
      let new_bit_mask = 1 lsl (t.bucket_count_log2 - 1) in
      let partition_fn elt_id =
        let elt = Arena.dereference t.arena elt_id in
        let new_index = elt_index t elt in
        (* Sanity check that the lower bits (below our new discriminator) refer to
           the old bucket in which we found this element. If not, the hash
           function must have returned a different value when we last placed this
           element. *)
        if new_index land lnot new_bit_mask <> old_index then
          raise_nondeterministic_hash
            "Element originally in bucket %d was rehashed to inconsistent \
             bucket %d during expansion to %d buckets"
            old_index new_index new_bucket_count;
        new_index land new_bit_mask = 0
      in
      Bucket.partition bucket ~f:partition_fn
    in
    fun t ->
      (* Scale the number of hashset buckets. *)
      let old_hashset = t.hashset in
      let new_bucket_count = 2 * Array.length old_hashset in
      let new_hashset = Array.make new_bucket_count (Bucket.empty ()) in
      t.hashset <- new_hashset;
      t.bucket_count_log2 <- t.bucket_count_log2 + 1;
      (* The bindings in each bucket will be split into two new buckets, using the
         next-highest bit of the element hash as a discriminator. *)
      let new_bit_mask = 1 lsl (t.bucket_count_log2 - 1) in
      Array.iteri old_hashset ~f:(fun old_index bucket ->
          let bucket_lower, bucket_upper =
            partition_bucket t ~new_bucket_count ~old_index bucket
          in
          new_hashset.(old_index) <- bucket_lower;
          new_hashset.(old_index lor new_bit_mask) <- bucket_upper);

      (* Scale the arena size. *)
      Arena.expand t.arena (arena_capacity ~bucket_count:new_bucket_count)

  let add t elt =
    if Arena.is_full t.arena then resize t;
    let arena_idx = Arena.allocate t.arena elt in
    (* Add the arena offset to the hashset. *)
    let elt_idx = elt_index t elt in
    t.hashset.(elt_idx) <- Bucket.cons arena_idx t.hashset.(elt_idx)

  let invariant invariant_elt t =
    assert (1 lsl t.bucket_count_log2 = Array.length t.hashset);
    Array.iteri t.hashset ~f:(fun index bucket ->
        Bucket.iter bucket ~f:(fun id ->
            let elt = Arena.dereference t.arena id in
            let recomputed_index =
              t.hash_elt elt land ((1 lsl t.bucket_count_log2) - 1)
            in
            assert (recomputed_index = index);
            invariant_elt elt))

  (* Using [Obj.reachable_words] directly on values of type [t] will give
     inaccurate results when using [--no-naked-pointers] or the OCaml Multicore
     runtime, since atoms (such as the empty array) are included in the count. As
     a workaround, we provide a dedicated [reachable_words] function for use in
     benchmarking this implementation. *)
  let reachable_words { arena; hash_elt; hashset; bucket_count_log2 = _ } =
    let record_words = 1 + 4 (* header + 4 fields *)
    and arena_words = Arena.reachable_words arena
    and hash_elt_words = Obj.reachable_words (Obj.repr hash_elt)
    and hashset_words =
      Array.fold_left hashset ~init:1 ~f:(fun acc bucket ->
          match Bucket.length bucket with
          | 0 | 1 ->
              (* inlined bucket element *)
              acc + 1
          | n ->
              (* pointer to allocated array with header *)
              acc + 1 + (1 + n))
    in
    record_words + arena_words + hash_elt_words + hashset_words
end
