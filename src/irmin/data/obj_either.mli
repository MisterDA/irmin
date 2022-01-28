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

(** A variant of the [Either.t] type that distinguishes [Left] and [Right] cases
    by their {i immediacy} (ie. whether [Obj.is_int] holds of the contents)
    rather than using a heap-allocated variant. As such, the runtime overhead of
    the [Obj_either] tag is zero.

    Misuse of the API (e.g. by calling {!of_immediate} with a non-immediate
    value) will result a runtime exception. Since immediacy is not tracked at
    the type level, you should only use this type with concrete left and right
    types that are known to have the correct immediacy (to avoid breaking the
    abstraction barrier on an arbitrary type).

    This module is not exposed for external use. *)

type ('a, 'b) t = private Obj.t

val of_immediate_exn : 'a -> ('a, _) t
val of_value_exn : 'b -> (_, 'b) t

type state = Immediate | Value

val inspect : (_, _) t -> state
val get_immediate_exn : ('a, _) t -> 'a
val get_value_exn : (_, 'b) t -> 'b
