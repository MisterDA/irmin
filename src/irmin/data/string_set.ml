type elt = string

type t = {
  elt_length : int;
  hash_elt : elt -> int;
  hash_elt_substring : Bigstringaf.t -> off:int -> len:int -> int;
  empty_slot : string;
      (** Pre-allocated string of \000s of the correct length *)
  mutable data : Bigstringaf.t;
  mutable data_length : int;
  mutable slot_count : int;
  mutable cardinal : int;
}

type hashset = t

module Slot : sig
  type t

  val of_elt : hashset -> elt -> t
  val of_elt_substring : hashset -> src:Bigstringaf.t -> src_off:int -> t
  val contains : hashset -> t -> string -> bool
  val is_empty : hashset -> t -> bool
  val get : hashset -> t -> elt
  val set : hashset -> t -> elt -> unit
  val set_substring : hashset -> t -> src:Bigstringaf.t -> src_off:int -> unit
  val next : hashset -> t -> t
  val iter_all : hashset -> f:(t -> unit) -> unit
  val to_offset : t -> int
end = struct
  type t = Offset of int [@@ocaml.unboxed]

  let offset_of_hash h hash =
    let index = (abs hash) mod h.slot_count in
    Offset (index * h.elt_length)

  let of_elt h elt = offset_of_hash h (h.hash_elt elt)

  let of_elt_substring h ~src ~src_off =
    offset_of_hash h (h.hash_elt_substring src ~off:src_off ~len:h.elt_length)

  let contains h (Offset offset) string =
    Bigstringaf.memcmp_string h.data offset string 0 h.elt_length = 0

  let is_empty h t = contains h t h.empty_slot

  let get h (Offset offset) =
    Bigstringaf.substring h.data ~off:offset ~len:h.elt_length

  let set h (Offset offset) elt =
    Bigstringaf.blit_from_string elt ~src_off:0 h.data ~dst_off:offset
      ~len:h.elt_length

  let set_substring h (Offset offset) ~src ~src_off =
    Bigstringaf.blit src ~src_off h.data ~dst_off:offset
      ~len:h.elt_length

  let next h (Offset offset) = Offset ((offset + h.elt_length) mod h.data_length)

  let iter_all hashset ~f =
    if hashset.data_length = 0 then ()
    else (
      f (Offset 0);
      let rec aux = function
        | Offset 0 -> ()
        | offset ->
            f offset;
            aux (next hashset offset)
      in
      aux (next hashset (Offset 0)))

  let to_offset (Offset n) = n
end

let empty_all_slots t =
  Slot.iter_all t ~f:(fun slot -> Slot.set t slot t.empty_slot)

let create ~initial_capacity ~elt_length ~hash:hash_elt
    ~hash_substring:hash_elt_substring () =
  let slot_count =
    let rec aux n =
      if n >= initial_capacity then ( n)
      else if n * 2 > Sys.max_array_length then ( n)
      else aux  (n * 2)
    in
    aux 2
  in
  let data_length = slot_count * elt_length in
  let data = Bigstringaf.create data_length in
  let empty_slot = String.make elt_length '\000' in
  let t =
    {
      data;
      data_length;
      hash_elt;
      hash_elt_substring;
      elt_length;
      empty_slot;
      slot_count;
      cardinal = 0;
    }
  in
  empty_all_slots t;
  t

let load_factor t =
  let slots_available = Bigstringaf.length t.data / t.elt_length in
  Float.of_int t.cardinal /. Float.of_int slots_available

let rec unguarded_add t slot elt =
  if Slot.is_empty t slot then
    (* Write the element to this slot *)
    Slot.set t slot elt
  else
    (* TODO: check that the slot doesn't already contain this element *)
    unguarded_add t (Slot.next t slot) elt

let rec unguarded_add_substring t slot ~src ~src_off =
  if Slot.is_empty t slot then
    (* Write the element to this slot *)
    Slot.set_substring t slot ~src ~src_off
  else unguarded_add_substring t (Slot.next t slot) ~src ~src_off

let resize t =
  let old_len = Bigstringaf.length t.data in
  let old_data = t.data in
  let new_len = old_len + (t.slot_count / 2) * t.elt_length in
  let new_data = Bigstringaf.create new_len in
  let old_t = { t with data = old_data; data_length = old_len } in
  t.data <- new_data;
  t.data_length <- new_len;
  t.slot_count <- new_len / t.elt_length;
  empty_all_slots t;
  Slot.iter_all old_t ~f:(fun old_slot ->
      if not (Slot.is_empty old_t old_slot) then
        let src_off = Slot.to_offset old_slot in
        let new_slot = Slot.of_elt_substring t ~src:old_t.data ~src_off in
        unguarded_add_substring t new_slot ~src:old_t.data ~src_off)

let add t elt =
  if String.length elt <> t.elt_length then
    failwith "Cannot write string of incorrect size to hashset";
  if String.equal elt t.empty_slot then
    failwith "Cannot write null value to hashset";
  if Float.compare (load_factor t) 0.9 >= 0 then resize t;

  let slot = Slot.of_elt t elt in
  unguarded_add t slot elt;
  t.cardinal <- t.cardinal + 1

let mem t elt =
  if String.length elt <> t.elt_length then
    failwith "Cannot read string of incorrect size from hashset";
  if String.equal elt t.empty_slot then
    failwith "Cannot read null value from hashset";

  let rec probe_loop slot =
    if Slot.contains t slot elt then true
    else if Slot.is_empty t slot then false
    else probe_loop (Slot.next t slot)
  in
  probe_loop (Slot.of_elt t elt)

let invariant invariant_elt t =
  let element_count = ref 0 in
  Slot.iter_all t ~f:(fun slot ->
      if not (Slot.is_empty t slot) then (
        incr element_count;
        invariant_elt (Slot.get t slot)));
  assert (t.cardinal = !element_count)

let reachable_words t =
  let bytes_per_word = Sys.word_size / 8 in
  (t.data_length / bytes_per_word) + Obj.reachable_words (Obj.repr t)
