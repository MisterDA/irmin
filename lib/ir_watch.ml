(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Log = Log.Make(struct let section = "WATCH" end)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type 'a diff = [`Updated of 'a * 'a | `Removed of 'a | `Added of 'a]

module type S = sig
  type key
  type value
  type watch
  type t
  val stats: t -> int * int
  val notify: t -> key -> value option -> unit Lwt.t
  val create: unit -> t
  val clear: t -> unit
  val watch_key: t -> key -> ?init:value -> (value diff -> unit Lwt.t) -> watch Lwt.t
  val watch: t -> ?init:(key * value) list -> (key -> value diff -> unit Lwt.t) ->
    watch Lwt.t
  val unwatch: t -> watch -> unit Lwt.t
  val listen_dir: t -> string
    -> key:(string -> key option)
    -> value:(key -> value option Lwt.t)
    -> unit
end

let listen_dir_hook =
  ref (fun _dir _fn ->
      Log.error "Listen hook not set!";
      assert false
    )

let set_listen_dir_hook fn =
  listen_dir_hook := fn

let id () =
  let c = ref 0 in
  fun () -> incr c; !c

let global = id ()

module Make (K: Tc.S0) (V: Tc.S0) = struct

  type key = K.t
  type value = V.t
  type watch = int
  module OV = Tc.Option(V)
  module KMap = Ir_misc.Map(K)
  module IMap = Ir_misc.Map(Tc.Int)

  type key_handler = value diff -> unit Lwt.t
  type all_handler = key -> value diff -> unit Lwt.t

  type t = {
    id: int;
    lock: Lwt_mutex.t;
    mutable next: int;
    mutable keys: (key * value option * key_handler) IMap.t;
    mutable all : (value KMap.t * all_handler) IMap.t;
    enqueue: ((unit -> unit Lwt.t) -> unit Lwt.t) Lazy.t;
  }

  let stats t = IMap.cardinal t.keys, IMap.cardinal t.all
  let to_string t = let k, a = stats t in Printf.sprintf "%d: %dk/%da" t.id k a
  let next t = let id = t.next in t.next <- id + 1; id

  let clear t =
    t.keys <- IMap.empty;
    t.all  <- IMap.empty;
    t.next <- 0

  let enqueue () = lazy (
    let stream, push = Lwt_stream.create () in
    Lwt.async (fun () -> Lwt_stream.iter_s (fun f -> f ()) stream);
    function v ->
      let t, u = Lwt.task () in
      let todo () = v () >|= Lwt.wakeup u in
      push (Some todo);
      t
  )

  let create () = {
    lock = Lwt_mutex.create (); enqueue = enqueue ();
    id = global (); next = 0;
    keys = IMap.empty; all = IMap.empty;
  }

  let unwatch_unsafe t id =
    let all  = IMap.remove id t.all in
    let keys = IMap.remove id t.keys in
    t.all  <- all;
    t.keys <- keys

  let unwatch t id =
    Lwt_mutex.with_lock t.lock (fun () ->
        unwatch_unsafe t id;
        Lwt.return_unit
      )

  let mk old value = match old, value with
    | None  , None   -> assert false
    | Some v, None   -> `Removed v
    | None  , Some v -> `Added v
    | Some x, Some y -> `Updated (x, y)

  let enqueue t = Lazy.force t.enqueue

  let notify_all t key value =
    let todo = ref [] in
    let all = IMap.fold (fun id (init, f as arg) acc ->
        let old_value = try Some (KMap.find key init) with Not_found -> None in
        if OV.equal old_value value then (
          Log.debug "notify-all: same value, skipping.";
          IMap.add id arg acc
        ) else (
          Log.debug "notify-all: firing %d.%d!" t.id id;
          todo := (fun () -> f key (mk old_value value)) :: !todo;
          let init = match value with
            | None   -> KMap.remove key init
            | Some v -> KMap.add key v init
          in
          IMap.add id (init, f) acc
        )
      ) t.all IMap.empty
    in
    t.all <- all;
    enqueue t (fun () -> Lwt_list.iter_p (fun x -> x ()) !todo)

  let notify_key t key value =
    let todo = ref [] in
    let keys = IMap.fold (fun id (k, old_value, f as arg) acc ->
        if not (K.equal key k) then IMap.add id arg acc
        else if OV.equal value old_value then (
          Log.debug "notify-key: same value, skipping.";
          IMap.add id arg acc
        ) else (
          Log.debug "notify-key: firing %d.%d!" t.id id;
          todo := (fun () -> f (mk old_value value)) :: !todo;
          IMap.add id (k, value, f) acc
        )
      ) t.keys IMap.empty
    in
    t.keys <- keys;
    enqueue t (fun () -> Lwt_list.iter_p (fun x -> x()) !todo)

  let notify t key value =
    Lwt.join [notify_all t key value; notify_key t key value]

  let watch_key_unsafe t key ?init f =
    Log.debug "watch-key %s" (to_string t);
    let id = next t in
    t.keys <- IMap.add id (key, init, f) t.keys;
    id

  let watch_key t key ?init f =
    Lwt_mutex.with_lock t.lock (fun () ->
        let id = watch_key_unsafe t ?init key f in
        Lwt.return id
      )

  let watch_unsafe t ?(init=[]) f =
    Log.debug "watch %s" (to_string t);
    let id = next t in
    t.all <- IMap.add id (KMap.of_alist init, f) t.all;
    id

  let watch t ?init f =
    Lwt_mutex.with_lock t.lock (fun () ->
        let id = watch_unsafe t ?init f in
        Lwt.return id
      )

  let listen_dir (t:t) dir ~key ~value =
    Log.debug "Add a listen hook for %s" (to_string t);
    !listen_dir_hook t.id dir (fun file ->
        Log.debug "listen_dir_hook: %s %s" (to_string t) file;
        match key file with
        | None     -> Lwt.return_unit
        | Some key -> value key >>= notify t key
      )

end
