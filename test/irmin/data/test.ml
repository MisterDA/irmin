open! Import

let () =
  Random.self_init ();
  Alcotest.run __FILE__
    [
      ("Arena", Test_arena.tests);
      ("Uniform_array", Test_uniform_array.tests);
      ("Obj_either", Test_obj_either.tests);
      ("Immediate_array", Test_immediate_array.tests);
      ("String_set", Test_string_set.tests);
    ]
