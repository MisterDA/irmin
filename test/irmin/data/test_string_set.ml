open! Import
module String_set = Irmin_data.Hashset.Fixed_size_string

let test_simple () =
  let set =
    String_set.create ~elt_length:1 ~initial_capacity:0 ~hash:Hashtbl.hash ()
  in
  String_set.mem set "a" |> check_bool __POS__ ~expected:false;
  String_set.add set "a";
  String_set.mem set "a" |> check_bool __POS__ ~expected:true;
  String_set.add set "b";
  String_set.add set "c";
  String_set.mem set "a" |> check_bool __POS__ ~expected:true;
  String_set.mem set "b" |> check_bool __POS__ ~expected:true;
  String_set.mem set "c" |> check_bool __POS__ ~expected:true

let test_random () =
  let elt_length = 8 in
  let set =
    String_set.create ~elt_length ~initial_capacity:31 ~hash:Hashtbl.hash ()
  in
  let reference_tbl = Stdlib.Hashtbl.create 0 in
  let reference_vector = Vector.create ~dummy:"" in
  let random_string () =
    String.init elt_length ~f:(fun _ -> char_of_int (Random.int 256))
  in
  for i = 0 to 100_000 do
    (* Add a new element: *)
    let new_elt = random_string () in
    String_set.add set new_elt;
    Stdlib.Hashtbl.add reference_tbl new_elt ();
    Vector.push reference_vector new_elt;

    (* Pick a random existing element and check [mem] is true: *)
    let elt = Vector.get reference_vector (Random.int (i + 1)) in
    assert (Stdlib.Hashtbl.mem reference_tbl elt);
    String_set.mem set elt |> check_bool __POS__ ~expected:true;

    (* Pick a random non-existing element and check [mem] is false: *)
    let non_elt = random_string () in
    assert (not (Stdlib.Hashtbl.mem reference_tbl non_elt));
    String_set.mem set non_elt |> check_bool __POS__ ~expected:false;

    (* Check that the internal invariants hold, and that all internal elements
       are also contained in the reference: *)
    if i mod 1_000 = 0 then
      String_set.invariant
        (fun elt ->
          check_bool __POS__ ~expected:true
            (Stdlib.Hashtbl.mem reference_tbl elt))
        set
  done

let test_nondeterministic_hash () =
  (* Initially the hash function puts all elements in to the final bucket: *)
  let hash_fn = ref (fun _ -> lnot 0) in
  let hash elt = !hash_fn elt in

  let set = String_set.create ~elt_length:1 ~initial_capacity:1 ~hash () in
  String_set.add set "a";
  String_set.add set "b";
  String_set.add set "c";
  String_set.add set "d";

  (* Change the hash function to put elements in the first bucket instead, then
     force a resize. *)
  (hash_fn := fun _ -> 0);
  try
    String_set.add set "e";
    Alcotest.fail "Adding 'e' should have raised [Nondeterministic_hash]"
  with
  | String_set.Nondeterministic_hash _ -> ()
  | exn -> raise exn

let tests =
  let test name fn = Alcotest.test_case name `Quick fn in
  [
    test "simple" test_simple;
    test "random" test_random;
    test "nondeterministic_hash" test_nondeterministic_hash;
  ]
