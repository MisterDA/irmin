open! Import
module Obj_either = Private.Obj_either

let test_immediate () =
  let t = Obj_either.of_immediate_exn 42 in
  assert (Obj_either.inspect t = Immediate);
  assert (Obj_either.get_immediate_exn t = 42);
  check_failure __POS__ (fun () ->
      Obj_either.of_immediate_exn "not_an_immediate");
  check_failure __POS__ (fun () -> Obj_either.get_value_exn t)

let test_value () =
  let t = Obj_either.of_value_exn [||] in
  assert (Obj_either.inspect t = Value);
  assert (Obj_either.get_value_exn t = [||]);
  check_failure __POS__ (fun () -> Obj_either.of_value_exn 42);
  check_failure __POS__ (fun () -> Obj_either.get_immediate_exn t)

let tests =
  let test name fn = Alcotest.test_case name `Quick fn in
  [ test "immediate" test_immediate; test "value" test_value ]
