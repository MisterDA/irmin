include Irmin_data

let check typ pos ~expected actual =
  Alcotest.(check ~pos typ) "" expected actual

let check_bool = check Alcotest.bool
let check_int = check Alcotest.int
let check_string = check Alcotest.string
let check_int_list = check Alcotest.(list int)
let check_int_array = check Alcotest.(array int)

let check_invalid_arg pos f =
  let fail got =
    Alcotest.failf ~pos
      "Expected function to raise `Invalid_argument`, but raised: %a"
      Fmt.(Dump.option exn)
      got
  in
  match f () with
  | _ -> fail None
  | exception Invalid_argument _ -> ()
  | exception exn -> fail (Some exn)
