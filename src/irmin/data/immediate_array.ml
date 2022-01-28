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

type 'a t = ('a, 'a array) Obj_either.t

let invariant invariant_elt t =
  match Obj_either.inspect t with
  | Immediate -> invariant_elt (Obj_either.get_immediate_exn t)
  | Value ->
      let arr = Obj_either.get_value_exn t in
      assert (Array.length arr <> 1);
      Array.iter ~f:invariant_elt arr

let to_array t =
  match Obj_either.inspect t with
  | Immediate -> [| Obj_either.get_immediate_exn t |]
  | Value -> Obj_either.get_value_exn t

let of_list = function
  | [ x ] -> Obj_either.of_immediate_exn x
  | l -> Obj_either.of_value_exn (Array.of_list l)

let of_list_rev = function
  | [ x ] -> Obj_either.of_immediate_exn x
  | [] -> Obj_either.of_value_exn [||]
  | a :: l ->
      let len = 1 + List.length l in
      let arr = Array.make len a in
      let r = ref l in
      (* We start at [len - 2] because we already put [a] at [t.(len - 1)]. *)
      for i = len - 2 downto 0 do
        match !r with
        | [] -> assert false
        | a :: l ->
            Array.unsafe_set arr i a;
            r := l
      done;
      Obj_either.of_value_exn arr

let to_list t =
  match Obj_either.inspect t with
  | Immediate -> [ Obj_either.get_immediate_exn t ]
  | Value -> Array.to_list (Obj_either.get_value_exn t)

let iter t ~f =
  match Obj_either.inspect t with
  | Immediate -> f (Obj_either.get_immediate_exn t)
  | Value -> Array.iter ~f (Obj_either.get_value_exn t)

let map t ~f =
  match Obj_either.inspect t with
  | Immediate ->
      Obj_either.of_immediate_exn (f (Obj_either.get_immediate_exn t))
  | Value -> Obj_either.of_value_exn (Array.map ~f (Obj_either.get_value_exn t))

let map_inplace t ~f =
  match Obj_either.inspect t with
  | Immediate ->
      Obj_either.of_immediate_exn (f (Obj_either.get_immediate_exn t))
  | Value ->
      Array.map_inplace ~f (Obj_either.get_value_exn t);
      t

let fold t ~f ~init =
  match Obj_either.inspect t with
  | Immediate -> f init (Obj_either.get_immediate_exn t)
  | Value -> Array.fold_left ~f ~init (Obj_either.get_value_exn t)

let exists t ~f =
  match Obj_either.inspect t with
  | Immediate -> f (Obj_either.get_immediate_exn t)
  | Value -> Array.exists ~f (Obj_either.get_value_exn t)

let length t =
  match Obj_either.inspect t with
  | Immediate -> 1
  | Value -> Array.length (Obj_either.get_value_exn t)

let unsafe_get t i =
  match Obj_either.inspect t with
  | Immediate -> Obj_either.get_immediate_exn t
  | Value -> Array.unsafe_get (Obj_either.get_value_exn t) i

let get t i =
  match Obj_either.inspect t with
  | Immediate ->
      if i <> 0 then
        Printf.ksprintf invalid_arg
          "Immediate_array.get: out of bounds index %d" i
      else Obj_either.get_immediate_exn t
  | Value -> Array.get (Obj_either.get_value_exn t) i

let unsafe_set t i x =
  match Obj_either.inspect t with
  | Immediate -> Obj_either.of_immediate_exn x
  | Value ->
      Array.unsafe_set (Obj_either.get_value_exn t) i x;
      t

let set t i x =
  match Obj_either.inspect t with
  | Immediate ->
      if i <> 0 then
        Printf.ksprintf invalid_arg
          "Immediate_array.set: out of bounds index %d" i
      else Obj_either.of_immediate_exn x
  | Value ->
      Array.set (Obj_either.get_value_exn t) i x;
      t

let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
  if len = 0 then dst
  else
    match Obj_either.inspect src with
    | Immediate ->
        let elt = Obj_either.get_immediate_exn src in
        assert (src_pos = 0);
        assert (len = 1);
        unsafe_set dst dst_pos elt
    | Value -> (
        let src = Obj_either.get_value_exn src in
        match Obj_either.inspect dst with
        | Immediate ->
            assert (dst_pos = 0);
            assert (len = 1);
            Obj_either.of_immediate_exn (Array.unsafe_get src src_pos)
        | Value ->
            let dst' = Obj_either.get_value_exn dst in
            Array.blit ~src ~src_pos ~dst:dst' ~dst_pos ~len;
            dst)

let singleton x = Obj_either.of_immediate_exn x

let create ~len x =
  if len = 1 then Obj_either.of_immediate_exn x
  else Obj_either.of_value_exn (Array.make len x)

let empty () = Obj_either.of_value_exn [||]

let init len ~f =
  match len with
  | n when n < 0 ->
      Printf.ksprintf invalid_arg
        "Immediate_array.init: negative array length %d" n
  | 0 -> empty ()
  | 1 -> singleton (f 0)
  | _ ->
      let e0 = f 0 in
      let t = create ~len e0 in
      for i = 1 to len - 1 do
        let _ = unsafe_set t i (f i) in
        ()
      done;
      t

let list_rev_partition =
  let rec part yes no ~f = function
    | [] -> (yes, no)
    | x :: l -> if f x then part ~f (x :: yes) no l else part ~f yes (x :: no) l
  in
  fun l ~f -> part ~f [] [] l

let partition : type a. a t -> f:(a -> bool) -> a t * a t =
 fun t ~f ->
  (* Partition function is optimised to avoid allocations for small array
     lengths as these are very common when using immediate arrays as hashtable
     buckets. *)
  match length t with
  | 0 -> (empty (), empty ())
  | 1 ->
      let elt = unsafe_get t 0 in
      if f elt then (singleton elt, empty ()) else (empty (), singleton elt)
  | 2 -> (
      let elt0 = unsafe_get t 0 and elt1 = unsafe_get t 1 in
      match (f elt0, f elt1) with
      | true, false -> (singleton elt0, singleton elt1)
      | false, true -> (singleton elt1, singleton elt0)
      | (true as both_left), true | (false as both_left), false ->
          let both = create ~len:2 elt0 in
          let both = unsafe_set both 1 elt1 in
          if both_left then (both, empty ()) else (empty (), both))
  | n when n < Sys.int_size ->
      (* If the bucket length is less than the number of bits in an integer,
         we can use the bits of an integer to record partitioning choices and
         then allocate arrays of precisely the correct size. *)
      let bitv = ref 0 in
      let popcount = ref 0 in
      for i = 0 to n - 1 do
        let elt = unsafe_get t i in
        if f elt then (
          bitv := !bitv lor (1 lsl i);
          incr popcount)
      done;
      let bitv = !bitv in
      let left_size = !popcount and right_size = n - !popcount in
      let dummy = unsafe_get t 0 in
      let left = ref (create ~len:left_size dummy)
      and right = ref (create ~len:right_size dummy) in
      let added_to_left = ref 0 in
      for i = 0 to n - 1 do
        match (bitv lsr i) land 1 with
        | 1 ->
            left := unsafe_set !left !added_to_left (unsafe_get t i);
            incr added_to_left
        | 0 -> right := unsafe_set !right (i - !added_to_left) (unsafe_get t i)
        | _ -> assert false
      done;
      (!left, !right)
  | _ ->
      let left, right = to_list t |> list_rev_partition ~f in
      (of_list_rev left, of_list_rev right)
