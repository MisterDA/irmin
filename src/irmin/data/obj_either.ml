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

type ('a, 'b) t = Obj.t

let of_immediate_exn : type a b. a -> (a, b) t =
 fun a ->
  if Obj.is_block (Obj.repr a) then
    failwith "Obj_either.of_immediate: passed a heap-allocated value";
  Obj.repr a

let of_value_exn : type a b. b -> (a, b) t =
 fun b ->
  if Obj.is_int (Obj.repr b) then
    failwith "Obj_either.of_value: passed an immediate value";
  Obj.repr b

type state = Immediate | Value

let inspect t = if Obj.is_int t then Immediate else Value

let get_immediate_exn : type a b. (a, b) t -> a =
 fun t ->
  match inspect t with
  | Immediate -> Obj.obj t
  | Value ->
      failwith "Obj_either.get_immediate_exn: passed a heap-allocated value"

let get_value_exn : type a b. (a, b) t -> b =
 fun t ->
  match inspect t with
  | Value -> Obj.obj t
  | Immediate -> failwith "Obj_either.get_value_exn: passed an immediate value"
