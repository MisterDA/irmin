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

type 'a t

val empty : _ t
val create : len:int -> 'a -> 'a t
val singleton : 'a -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val length : _ t -> int
val get : 'a t -> int -> 'a
val unsafe_get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val unsafe_set : 'a t -> int -> 'a -> unit
val swap : _ t -> int -> int -> unit
val map : 'a t -> f:('a -> 'b) -> 'b t
val map_inplace : 'a t -> f:('a -> 'a) -> unit
val iter : 'a t -> f:('a -> unit) -> unit
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
val exists : 'a t -> f:('a -> bool) -> bool
val of_array : 'a array -> 'a t
val to_array : 'a t -> 'a array
val of_list : 'a list -> 'a t
val of_list_rev : 'a list -> 'a t
val to_list : 'a t -> 'a list

(** {2 Extra lowlevel and unsafe functions} *)

val unsafe_blit :
  src:'a t -> src_pos:int -> dst:'a t -> dst_pos:int -> len:int -> unit

val unsafe_create_uninitialized : len:int -> _ t
(** The behavior is undefined if you access an element before setting it. *)

val create_obj_array : len:int -> Obj.t t
(** New obj array filled with [Obj.repr 0] *)

val unsafe_set_assuming_currently_int : Obj.t t -> int -> Obj.t -> unit
(** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to [obj],
    but only works correctly if the value there is an immediate, i.e.
    [Obj.is_int (get t i)]. This precondition saves a dynamic check.

    [unsafe_set_int_assuming_currently_int] is similar, except the value being
    set is an int.

    [unsafe_set_int] is similar but does not assume anything about the target. *)

val unsafe_set_int_assuming_currently_int : Obj.t t -> int -> int -> unit
val unsafe_set_int : Obj.t t -> int -> int -> unit

include Invariant.S1 with type 'a t := 'a t
