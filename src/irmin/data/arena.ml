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

type internal = {
  elt_length : int;
  mutable data : Bigstringaf.t;
  mutable next_offset : int;
}

type t = internal

(* An offset into a data bigstring. *)
type id = int

let create ~elt_length ~initial_capacity =
  {
    elt_length;
    data = Bigstringaf.create (elt_length * initial_capacity);
    next_offset = 0;
  }

let elt_equal t offset elt =
  Bigstringaf.memcmp_string t.data offset elt 0 t.elt_length = 0

let is_full t = t.next_offset = Bigstringaf.length t.data

let allocate t elt =
  if is_full t then invalid_arg "Arena.allocate: arena is full";
  (* Write the element to the next available arena offset. *)
  let offset = t.next_offset in
  Bigstringaf.blit_from_string elt ~src_off:0 t.data ~dst_off:offset
    ~len:t.elt_length;
  t.next_offset <- offset + t.elt_length;
  offset

let dereference t off =
  if off + t.elt_length > t.next_offset then
    invalid_arg "Arena.dereference: reference doesn't belong to this arena";
  Bigstringaf.substring t.data ~off ~len:t.elt_length

let expand t size =
  let old_len = Bigstringaf.length t.data in
  let new_len = size * t.elt_length in
  if new_len < old_len then
    invalid_arg "Arena.expand: can't reduce the size of an existing arena";
  let new_data = Bigstringaf.create new_len in
  Bigstringaf.blit t.data ~src_off:0 new_data ~dst_off:0 ~len:t.next_offset;
  t.data <- new_data

let reachable_words t =
  let bytes_per_word = Sys.word_size / 8 in
  (* Doesn't account for any the fixed-size components of the bigarray struct,
     which are assumed to be negligible compared to the data itself. *)
  Bigstringaf.length t.data / bytes_per_word
