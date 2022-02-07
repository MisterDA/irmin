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

type t

val create : elt_length:int -> initial_capacity:int -> t
(** [create ~elt_length:len ~initial_capacity:n] is an empty arena of strings of
    length [len], with space sufficient to store [n] values. *)

val is_full : t -> bool
(** [is_full t] is true iff [t] has no remaining space for elements. *)

val expand : t -> int -> unit
(** [expand t n] re-allocates arena [t] to support storing up to [n]-many
    elements. Existing elements in the arena retain their original {!id}s.

    @raise Invalid_argument if [n] is less than the current capacity of [t]. *)

type id [@@ocaml.immediate]
(** The type of references to allocated elements in an arena. *)

val allocate : t -> string -> id
(** [allocate t s] adds the string [s] to arena [t], returning a reference to
    the storage location that may later be {!dereference}d to get back [s].

    @raise Invalid_argument
      if [t] {!is_full}. The behaviour is undefined if the length of [s] is not
      equal to the [elt_length] of [t]. *)

val dereference : t -> id -> string
(** [dereference t id] is the string that was passed to the {!allocate} call
    that returned [id]. The behaviour is undefined if [id] was not created by an
    allocation within [t]. *)

val elt_equal : t -> id -> string -> bool
(** [elt_equal t id s] is equivalent to [String.equal (dereference t id) s], but
    more efficient. *)

val reachable_words : t -> int
