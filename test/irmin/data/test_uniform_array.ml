open! Import
open Uniform_array

let zero_obj = Obj.repr (0 : int)
let phys_equal = ( == )
let does_raise f = match ignore (f ()) with () -> false | exception _ -> true

let test_create_obj_array () =
  let t = create_obj_array ~len:0 in
  assert (length t = 0)

let test_create () =
  let str = Obj.repr "foo" in
  let t = create ~len:2 str in
  assert (phys_equal (get t 0) str);
  assert (phys_equal (get t 1) str)

let test_init () =
  let t = init 0 ~f:(fun _ -> assert false) in
  assert (length t = 0);

  let t = init 5 ~f:Fun.id |> to_list in
  Alcotest.(check ~pos:__POS__ (list int)) "" [ 0; 1; 2; 3; 4 ] t;

  check_invalid_arg __POS__ (fun () -> init (-1) ~f:(fun _ -> assert false))

let test_of_list_rev () =
  let test pos ~len =
    let input = List.init ~len ~f:Fun.id in
    let expected = List.rev input in
    let actual = of_list_rev input in
    check_int_list pos ~expected (to_list actual);
    if len > 0 then check_int pos ~expected:(len - 1) (get actual 0)
  in
  test __POS__ ~len:0;
  test __POS__ ~len:5

let test_to_array () =
  let expected = Array.init 10 ~f:Fun.id in
  let actual = to_array (init 10 ~f:Fun.id) in
  check_int_array __POS__ ~expected actual

let test_of_array () =
  let len = 10 in
  let t = Array.init len ~f:Fun.id |> of_array in
  for i = 0 to len - 1 do
    check_int __POS__ ~expected:i (get t i)
  done

let test_float_elements () =
  let float = Obj.repr 3.5 in
  let t = create ~len:2 float in
  assert (Obj.tag (Obj.repr t) = 0);
  (* not a double array *)
  assert (phys_equal (get t 0) float);
  assert (phys_equal (get t 1) float);
  set t 1 (Obj.repr 4.);
  assert (Float.equal (Obj.obj (get t 1)) 4.)

let test_empty () =
  assert (length empty = 0);
  assert (does_raise (fun () -> get empty 0))

let test_singleton () =
  assert (length (singleton zero_obj) = 1);
  assert (phys_equal (get (singleton zero_obj) 0) zero_obj);
  assert (does_raise (fun () -> get (singleton zero_obj) 1));

  let f = 13. in
  let t = singleton (Obj.repr f) in
  invariant ignore t;
  assert (Obj.repr f = get t 0)

let test_get_and_set () =
  let t = create_obj_array ~len:1 in
  assert (length t = 1);
  assert (phys_equal (get t 0) zero_obj);
  assert (phys_equal (unsafe_get t 0) zero_obj);
  let one_obj = Obj.repr (1 : int) in
  let check_get expect =
    assert (phys_equal (get t 0) expect);
    assert (phys_equal (unsafe_get t 0) expect)
  in
  (* set *)
  set t 0 one_obj;
  check_get one_obj;

  (* unsafe_set *)
  unsafe_set t 0 zero_obj;
  check_get zero_obj;

  (* unsafe_set_assuming_currently_int *)
  unsafe_set_assuming_currently_int t 0 zero_obj;
  check_get zero_obj;
  unsafe_set_assuming_currently_int t 0 (Obj.repr 1.);
  check_get (Obj.repr 1.);

  (* unsafe_set_int *)
  unsafe_set_int t 0 1;
  check_get one_obj;
  unsafe_set_int t 0 0;
  check_get zero_obj;

  (* unsafe_set_int_assuming_currently_int *)
  unsafe_set_int_assuming_currently_int t 0 1;
  check_get one_obj

let test_swap () =
  let t = init 2 ~f:string_of_int in
  swap t 0 1;
  assert (to_list t = [ "1"; "0" ]);
  swap t 0 0;
  assert (to_list t = [ "1"; "0" ]);
  check_invalid_arg __POS__ (fun () -> swap t (-1) 0)

let test_map () =
  let len = 5 in
  let input = init len ~f:Fun.id in
  let actual = map input ~f:(( * ) 2) in
  let expected = Array.init len ~f:(( * ) 2) in
  check_int_array __POS__ ~expected (to_array actual);
  map_inplace input ~f:(( * ) 2);
  check_int_array __POS__ ~expected (to_array input)

let test_exists () =
  let test arr f = of_list arr |> exists ~f in
  let r pos expected actual = Alcotest.(check ~pos bool) "" expected actual in
  r __POS__ false (test [] Fun.id);
  r __POS__ true (test [ true ] Fun.id);
  r __POS__ true (test [ false; false; false; false; true ] Fun.id);
  r __POS__ true (test [ 0; 1; 2; 3; 4 ] (fun i -> i mod 2 = 1));
  r __POS__ false (test [ 0; 2; 4; 6; 8 ] (fun i -> i mod 2 = 1))

let test_iteri () =
  let test pos ~expected arr =
    let acc = ref [] in
    of_list arr |> iteri ~f:(fun i x -> acc := (i, x) :: !acc);
    Alcotest.(check ~pos (list (pair int char))) "" expected (List.rev !acc)
  in
  test __POS__ ~expected:[] [];
  test __POS__ ~expected:[ (0, 'a') ] [ 'a' ];
  test __POS__
    ~expected:[ (0, 'a'); (1, 'b'); (2, 'c'); (3, 'd') ]
    [ 'a'; 'b'; 'c'; 'd' ]

let test_fold () =
  fold ~f:(fun () _ -> assert false) ~init:() empty;
  fold ~f:( ^ ) ~init:"_" (of_list [ "a"; "b"; "c" ])
  |> check_string __POS__ ~expected:"_abc"

let test_unsafe_blit () =
  let check pos ~expected actual =
    Alcotest.(check ~pos (list string)) "" expected (to_list actual)
  in

  let () =
    let a = of_list [ "a0"; "a1"; "a2"; "a3"; "a4" ]
    and b = of_list [ "b0"; "b1"; "b2"; "b3"; "b4" ] in
    unsafe_blit ~src:a ~dst:b ~src_pos:0 ~dst_pos:1 ~len:3;
    check __POS__ ~expected:[ "a0"; "a1"; "a2"; "a3"; "a4" ] a;
    check __POS__ ~expected:[ "b0"; "a0"; "a1"; "a2"; "b4" ] b
  in

  (* Tests when [src == dst] and the source / destination regions overlap: *)
  let () =
    let a = of_list [ "0"; "1"; "2"; "3"; "4"; "5" ] in
    unsafe_blit ~src:a ~dst:a ~src_pos:0 ~dst_pos:1 ~len:4;
    check __POS__ ~expected:[ "0"; "0"; "1"; "2"; "3"; "5" ] a;

    let a = of_list [ "0"; "1"; "2"; "3"; "4"; "5" ] in
    unsafe_blit ~src:a ~dst:a ~src_pos:1 ~dst_pos:0 ~len:4;
    check __POS__ ~expected:[ "1"; "2"; "3"; "4"; "4"; "5" ] a
  in

  ()

let tests =
  let test name fn = Alcotest.test_case name `Quick fn in
  [
    test "create_obj_array" test_create_obj_array;
    test "create" test_create;
    test "init" test_init;
    test "of_list_rev" test_of_list_rev;
    test "to_array" test_to_array;
    test "of_array" test_of_array;
    test "float_elements" test_float_elements;
    test "empty" test_empty;
    test "singleton" test_singleton;
    test "get_and_set" test_get_and_set;
    test "swap" test_swap;
    test "map" test_map;
    test "exists" test_exists;
    test "iteri" test_iteri;
    test "fold" test_fold;
    test "unsafe_blit" test_unsafe_blit;
  ]
