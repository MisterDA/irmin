(*
 * Copyright (c) 2016-2020 Jane Street Group, LLC <opensource@janestreet.com>
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

(** Code in this module has been extracted from Jane Street's [base] library,
    with minimal modifications. *)

open! Import

(* WARNING:
   We use non-memory-safe things throughout the [Trusted] module.
   Most of it is only safe in combination with the type signature (e.g. exposing
   [val copy : 'a t -> 'b t] would be a big mistake). *)
module Trusted : sig
  type 'a t

  val empty : _ t
  val unsafe_create_uninitialized : len:int -> _ t
  val create_obj_array : len:int -> _ t
  val create : len:int -> 'a -> 'a t
  val singleton : 'a -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val swap : _ t -> int -> int -> unit
  val unsafe_get : 'a t -> int -> 'a
  val unsafe_set : 'a t -> int -> 'a -> unit
  val unsafe_set_int : _ t -> int -> int -> unit
  val unsafe_set_int_assuming_currently_int : _ t -> int -> int -> unit
  val unsafe_set_assuming_currently_int : _ t -> int -> Obj.t -> unit
  val length : _ t -> int

  val unsafe_blit :
    src:'a t -> src_pos:int -> dst:'a t -> dst_pos:int -> len:int -> unit

  val invariant : _ t -> unit
end = struct
  type 'a t = Obj_array.t

  let empty = Obj_array.empty
  let unsafe_create_uninitialized ~len = Obj_array.create_zero ~len
  let create_obj_array ~len = Obj_array.create_zero ~len
  let create ~len x = Obj_array.create ~len (Obj.repr x)
  let singleton x = Obj_array.singleton (Obj.repr x)
  let swap t i j = Obj_array.swap t i j
  let get arr i = Obj.obj (Obj_array.get arr i)
  let set arr i x = Obj_array.set arr i (Obj.repr x)
  let unsafe_get arr i = Obj.obj (Obj_array.unsafe_get arr i)
  let unsafe_set arr i x = Obj_array.unsafe_set arr i (Obj.repr x)
  let unsafe_set_int arr i x = Obj_array.unsafe_set_int arr i x

  let unsafe_set_int_assuming_currently_int arr i x =
    Obj_array.unsafe_set_int_assuming_currently_int arr i x

  let unsafe_set_assuming_currently_int arr i x =
    Obj_array.unsafe_set_assuming_currently_int arr i (Obj.repr x)

  let length = Obj_array.length
  let unsafe_blit = Obj_array.unsafe_blit
  let invariant = Obj_array.invariant
end

include Trusted

let init l ~f =
  if l < 0 then invalid_arg "Uniform_array.init"
  else
    let res = unsafe_create_uninitialized ~len:l in
    for i = 0 to l - 1 do
      unsafe_set res i (f i)
    done;
    res

let of_array arr = init ~f:(Array.unsafe_get arr) (Array.length arr)
let map a ~f = init ~f:(fun i -> f (unsafe_get a i)) (length a)

let map_inplace a ~f =
  for i = 0 to length a - 1 do
    unsafe_set a i (f (unsafe_get a i))
  done

let iter a ~f =
  for i = 0 to length a - 1 do
    f (unsafe_get a i)
  done

let iteri a ~f =
  for i = 0 to length a - 1 do
    f i (unsafe_get a i)
  done

let invariant inv_a t =
  invariant t;
  iter t ~f:inv_a

let to_list t = List.init ~f:(get t) ~len:(length t)

let of_list l =
  let len = List.length l in
  let res = unsafe_create_uninitialized ~len in
  List.iteri l ~f:(fun i x -> set res i x);
  res

let of_list_rev l =
  match l with
  | [] -> empty
  | a :: l ->
      let len = 1 + List.length l in
      let t = create ~len a in
      let r = ref l in
      (* We start at [len - 2] because we already put [a] at [t.(len - 1)]. *)
      for i = len - 2 downto 0 do
        match !r with
        | [] -> assert false
        | a :: l ->
            unsafe_set t i a;
            r := l
      done;
      t

(* It is not safe for [to_array] to be the identity function because [float
   array] may have a flat representation. *)
let to_array t = Array.init (length t) ~f:(fun i -> unsafe_get t i)

let exists t ~f =
  let rec loop t ~f i =
    if i < 0 then false else f (unsafe_get t i) || loop t ~f (i - 1)
  in
  loop t ~f (length t - 1)

let fold t ~init ~f =
  let r = ref init in
  for i = 0 to length t - 1 do
    r := f !r (unsafe_get t i)
  done;
  !r
