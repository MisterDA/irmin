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

(** This module is not exposed for external use, and is only here for the
    implementation of [Uniform_array] internally. [Obj.t Uniform_array.t] should
    be used in place of [Obj_array.t]. *)

open! Import

type t

val create : len:int -> Obj.t -> t
(** [create ~len x] returns an obj-array of length [len], all of whose indices
    have value [x]. *)

val create_zero : len:int -> t
(** [create_zero ~len] returns an obj-array of length [len], all of whose
    indices have value [Obj.repr 0]. *)

val singleton : Obj.t -> t
val empty : t
val length : t -> int

val get : t -> int -> Obj.t
(** [get t i] and [unsafe_get t i] return the object at index [i]. [set t i o]
    and [unsafe_set t i o] set index [i] to [o]. In no case is the object
    copied. The [unsafe_*] variants omit the bounds check of [i]. *)

val unsafe_get : t -> int -> Obj.t
val set : t -> int -> Obj.t -> unit
val unsafe_set : t -> int -> Obj.t -> unit
val swap : t -> int -> int -> unit

val unsafe_set_assuming_currently_int : t -> int -> Obj.t -> unit
(** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to [obj],
    but only works correctly if [Obj.is_int (get t i)]. This precondition saves
    a dynamic check.

    [unsafe_set_int_assuming_currently_int] is similar, except the value being
    set is an int.

    [unsafe_set_int] is similar but does not assume anything about the target. *)

val unsafe_set_int_assuming_currently_int : t -> int -> int -> unit
val unsafe_set_int : t -> int -> int -> unit

val unsafe_blit :
  src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit

include Invariant.S with type t := t
