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

module type S = sig
  type 'a t
  type 'a elt

  val add : 'a t -> 'a elt -> unit
  (** [add t elt] adds [elt] to [t]. *)

  val mem : 'a t -> 'a elt -> bool
  (** [mem t elt] is true iff the string [elt] has been added to [t]. *)

  val reachable_words : _ t -> int
  (** [reachable_words t] is the total number of heap words kept alive by [t]. *)

  val invariant : 'a elt Invariant.t -> 'a t Invariant.t
end

module type Sigs = sig
  module type S = S

  (** A hashset optimised for containing strings of fixed length. *)
  module Fixed_size_string : sig
    type t

    val create :
      elt_length:int ->
      initial_capacity:int ->
      hash:(string -> int) ->
      unit ->
      t
    (** [create ~elt_length:len ~initial_capacity:n ~hash:h ()] is a set of
        strings of length [len], capable of storing [n] values without internal
        reallocation, using [h] to determine the internal placement of elements. *)

    exception Nondeterministic_hash of string
    (** The exception raised when a hash function passed to [create] is found to
        behave non-deterministically (returning multiple outputs for a given
        element) when resizing the hashset. *)

    include S with type _ t := t and type _ elt := string
  end
end
