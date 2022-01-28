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

type 'a t = 'a -> unit
type 'a inv = 'a t

module type S = sig
  type t

  val invariant : t inv
end

module type S1 = sig
  type 'a t

  val invariant : 'a inv -> 'a t inv
end

module type S2 = sig
  type ('a, 'b) t

  val invariant : 'a inv -> 'b inv -> ('a, 'b) t inv
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val invariant : 'a inv -> 'b inv -> 'c inv -> ('a, 'b, 'c) t inv
end

module type Intf = sig
  type 'a t = 'a -> unit

  module type S = S
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3
end
