open! Import

let () =
  Random.self_init ();
  Alcotest.run __FILE__ [ ("String_set", Test_string_set.tests) ]
