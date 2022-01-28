let stabilize_garbage_collector () =
  let rec go fail last_heap_live_words =
    if fail <= 0 then
      failwith "Unable to stabilize the number of live words in the major heap";
    Gc.compact ();
    let stat = Gc.stat () in
    if stat.Gc.live_words <> last_heap_live_words then
      go (fail - 1) stat.Gc.live_words
  in
  go 10 0

let allocated_words () =
  let s = Gc.quick_stat () in
  s.minor_words +. s.major_words -. s.promoted_words

let open_stat_file name =
  let stat_file =
    let rnd = Random.bits () land 0xFFFFFF in
    let ( / ) = Filename.concat in
    "_build" / Printf.sprintf "%s-%06x.csv" name rnd
  in
  Printf.printf "Sending stats to '%s'\n%!" stat_file;
  let out = open_out stat_file in
  Printf.fprintf out
    "entries,implementation,reachable_words,allocated_words,time(ns)\n";
  out

(** Compute metrics of various hashset implementations, as a function of the
    number of entries:

    - total size in memory
    - extra allocations per entry
    - cost of [add] per entry

    Stats are emitted to a trace file to be interpreted by [analysis/main.py]. *)

module type S = sig
  type t
  type elt := string

  val implementation_name : string
  val create : unit -> t
  val add : t -> elt -> unit
  val reachable_words : t -> int
end

module Stringset_irmin : S = struct
  module T = Irmin_data.Hashset.Fixed_size_string

  type t = T.t

  let implementation_name = "irmin"

  let create () =
    T.create ~elt_length:32 ~hash:Hashtbl.hash ~initial_capacity:0 ()

  let add = T.add
  let reachable_words = T.reachable_words
end

module Stringset_stdlib : S = struct
  module T = Stdlib.Hashtbl

  type t = (string, unit) T.t

  let implementation_name = "stdlib"
  let create () = T.create 0
  let add t k = T.add t k ()
  let reachable_words (t : t) = Obj.reachable_words (Obj.repr t)
end

let random_string () =
  let b = Bytes.create 32 in
  for i = 0 to 31 do
    Bytes.set b i (Char.chr (Random.int 256))
  done;
  Bytes.unsafe_to_string b

let run_loop ~out (module Hashset : S) =
  let t = Hashset.create ()
  and iterations = 300_000
  and start_time = Mtime_clock.counter ()
  and last = ref Mtime.Span.zero
  and initial_allocations = allocated_words () in
  stabilize_garbage_collector ();
  for i = 1 to iterations do
    Hashset.add t (random_string ());
    if i mod 1_000 = 0 then (
      let time = Mtime_clock.count start_time in
      let diff = Mtime.Span.abs_diff time !last in
      let reachable_words = Hashset.reachable_words t in
      Printf.eprintf "\r%s : %#d / %#d%!" Hashset.implementation_name i
        iterations;
      Printf.fprintf out "%d,%s,%d,%f,%Ld\n%!" i Hashset.implementation_name
        reachable_words
        (allocated_words () -. initial_allocations)
        (Int64.div (Mtime.Span.to_uint64_ns diff) 1_000L);
      last := Mtime_clock.count start_time)
  done;
  Printf.eprintf "\r%s : done\x1b[K\n%!" Hashset.implementation_name

let () =
  let out = open_stat_file "hashset-memory-usage" in
  List.iter (run_loop ~out)
    [ (module Stringset_irmin); (module Stringset_stdlib) ];
  Printf.printf "\nDone\n"
