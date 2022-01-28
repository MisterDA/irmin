open! Import
module I = Immediate_array

let get_size (t : _ I.t) = 1 + Obj.reachable_words (Obj.repr t)

let test_empty () =
  let t = I.empty () in
  I.invariant (fun _ -> assert false) t

let test_singleton () =
  let t = I.singleton () in
  I.invariant ignore t

let test_init () =
  let t = I.init 5 ~f:Fun.id in
  for i = 0 to 4 do
    check_int __POS__ ~expected:i (I.get t i)
  done;
  check_invalid_arg __POS__ (fun () -> I.init (-1) ~f:(fun _ -> assert false))

let test_get_and_set () =
  let test ~len =
    let t = I.create ~len 0 in
    check_int __POS__ ~expected:len (I.length t);
    check_int __POS__ ~expected:0 (I.get t 0);
    check_int __POS__ ~expected:0 (I.unsafe_get t 0);
    let check_get pos t expected =
      check_int pos ~expected (I.get t 0);
      check_int pos ~expected (I.unsafe_get t 0)
    in
    let t = I.set t 0 1 in
    check_get __POS__ t 1;
    let t = I.unsafe_set t 0 0 in
    check_get __POS__ t 0
  in
  test ~len:1;
  test ~len:5;
  check_invalid_arg __POS__ (fun () -> I.(get (singleton ()) 2));
  check_invalid_arg __POS__ (fun () -> I.(set (singleton ()) 2 ()))

let test_length () =
  let check_size pos ~expected t =
    List.mem (get_size t) ~set:expected
    |> Alcotest.(check ~pos bool)
         (Fmt.str "Expected one of %a" Fmt.(Dump.list int) expected)
         true
  in
  let expected_size_of_length = function
    | 0 ->
        (* NOTE: when using [--disable-naked-pointers] (or the multicore OCaml
           runtime), atoms like the empty array are included in [Obj.reachable_words],
           making the "size" of an empty immediate array is [2] rather than [1]. *)
        [ 1; 2 ]
    | 1 -> [ 1 ]
    | n -> [ 2 + n ]
  in
  for i = 0 to 10 do
    let l = List.init ~len:i ~f:(fun _ -> ()) in
    let t = I.of_list l in
    check_int __POS__ ~expected:i (I.length t);
    check_size __POS__ ~expected:(expected_size_of_length i) t;
    I.invariant ignore t
  done

let test_partition () =
  let check pos ~expected actual =
    Alcotest.(check ~pos (pair (list int) (list int))) "" expected actual
  in

  let partition ~f t =
    let a, b = I.partition ~f t in
    I.invariant ignore a;
    I.invariant ignore b;
    (I.to_list a, I.to_list b)
  in

  partition ~f:(fun _ -> assert false) I.(empty ())
  |> check __POS__ ~expected:([], []);

  partition ~f:(fun i -> i = 1) (I.of_list [ 1 ])
  |> check __POS__ ~expected:([ 1 ], []);

  let test_partition_by_parity pos ~len =
    let parity i = i mod 2 = 0 in
    let test_arr = I.init ~f:Fun.id len in
    let evens = List.init ~len:((len + 1) / 2) ~f:(fun i -> 2 * i) in
    let odds = List.init ~len:(len / 2) ~f:(fun i -> (2 * i) + 1) in
    partition test_arr ~f:parity |> check pos ~expected:(evens, odds)
  in
  test_partition_by_parity __POS__ ~len:2;
  test_partition_by_parity __POS__ ~len:3;
  test_partition_by_parity __POS__ ~len:4;
  test_partition_by_parity __POS__ ~len:5;
  test_partition_by_parity __POS__ ~len:1_000;

  ()

let test_map () =
  let test pos ~len =
    let input = I.init len ~f:Fun.id in
    let actual = I.map input ~f:(( * ) 2) in
    let expected = Array.init len ~f:(( * ) 2) in
    check_int_array pos ~expected (I.to_array actual);

    let actual = I.map_inplace input ~f:(( * ) 2) in
    check_int_array pos ~expected (I.to_array actual)
  in
  test __POS__ ~len:0;
  test __POS__ ~len:1;
  test __POS__ ~len:5

let test_fold () =
  let concat_ints a b = int_of_string (string_of_int a ^ string_of_int b) in

  I.fold ~f:(fun () _ -> assert false) ~init:() (I.empty ());
  I.fold ~f:concat_ints ~init:1 (I.of_list [ 2 ])
  |> check_int __POS__ ~expected:12;
  I.fold ~f:concat_ints ~init:1 (I.of_list [ 2; 3; 4 ])
  |> check_int __POS__ ~expected:1234

let test_to_array () =
  let test pos ~len =
    let expected = Array.init len ~f:Fun.id in
    let actual = I.init len ~f:Fun.id |> I.to_array in
    check_int_array pos ~expected actual
  in
  test __POS__ ~len:0;
  test __POS__ ~len:1;
  test __POS__ ~len:5

let test_of_list_rev () =
  let test pos ~len =
    let input = List.init ~len ~f:Fun.id in
    let expected = List.rev input in
    let actual = I.of_list_rev input in
    check_int_list pos ~expected (I.to_list actual);
    if len > 0 then (
      (* Check that the first and last elements are as expected: *)
      check_int pos ~expected:(len - 1) I.(get actual 0);
      check_int pos ~expected:0 (I.get actual (len - 1)))
  in
  test __POS__ ~len:0;
  test __POS__ ~len:1;
  test __POS__ ~len:5

let test_unsafe_blit () =
  let test pos ~src_len ~dst_len =
    let src = I.init src_len ~f:(fun x -> -(x + 1))
    and dst = I.init dst_len ~f:(fun x -> x + 1) in
    let actual = I.unsafe_blit ~src ~dst ~src_pos:0 ~dst_pos:0 ~len:1 in
    let expected = -1 :: List.init ~len:(dst_len - 1) ~f:(fun x -> x + 2) in
    check_int_list pos ~expected (I.to_list actual)
  in

  (* Test valid blits for all 4 possible combinations of [src] and [dst] memory
     representations: *)
  test __POS__ ~src_len:1 ~dst_len:1;
  test __POS__ ~src_len:1 ~dst_len:5;
  test __POS__ ~src_len:5 ~dst_len:1;
  test __POS__ ~src_len:5 ~dst_len:5

let tests =
  let test name fn = Alcotest.test_case name `Quick fn in
  [
    test "empty" test_empty;
    test "singleton" test_singleton;
    test "init" test_init;
    test "get_and_set" test_get_and_set;
    test "length" test_length;
    test "partition" test_partition;
    test "map" test_map;
    test "fold" test_fold;
    test "to_array" test_to_array;
    test "of_list_rev" test_of_list_rev;
    test "unsafe_blit" test_unsafe_blit;
  ]
