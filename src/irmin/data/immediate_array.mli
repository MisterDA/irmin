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

(** An {i immediate} array is an array of elements that {i always} have an
    immediate runtime representation (i.e. [Obj.is_int] always holds).

    This requirement enables an array implementation that can represent
    singleton arrays as immediates (using 1 word rather than 3). A consequence
    of this is that the API is not purely mutable: functions that "mutate" an
    array must return that array (to cover the case of the array being an
    immediate singleton).

    This module is not exposed for external use. *)

type 'a t

val empty : unit -> _ t
val create : len:int -> 'a -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val of_list_rev : 'a list -> 'a t
val iter : 'a t -> f:('a -> unit) -> unit
val map : 'a t -> f:('a -> 'b) -> 'b t
val map_inplace : 'a t -> f:('a -> 'a) -> 'a t
val partition : 'a t -> f:('a -> bool) -> 'a t * 'a t
val fold : 'a t -> f:('acc -> 'a -> 'acc) -> init:'acc -> 'acc
val exists : 'a t -> f:('a -> bool) -> bool
val to_array : 'a t -> 'a array
val length : _ t -> int
val singleton : 'a -> 'a t

val unsafe_blit :
  src:'a t -> src_pos:int -> dst:'a t -> dst_pos:int -> len:int -> 'a t

val set : 'a t -> int -> 'a -> 'a t
val get : 'a t -> int -> 'a
val unsafe_set : 'a t -> int -> 'a -> 'a t
val unsafe_get : 'a t -> int -> 'a

include Invariant.S1 with type 'a t := 'a t
