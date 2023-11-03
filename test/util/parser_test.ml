module P = Util.Parser

let%expect_test "signed_integer works with positive signs" =
  let result = P.parse_exn P.signed_integer "+10" in
  Printf.printf "%i" result;
  [%expect {| 10 |}]
;;

let%expect_test "signed_integer works with negative signs" =
  let result = P.parse_exn P.signed_integer "-10" in
  Printf.printf "%i" result;
  [%expect {| -10 |}]
;;

let%expect_test "signed_integer works with no signs" =
  let result = P.parse_exn P.signed_integer "10" in
  Printf.printf "%i" result;
  [%expect {| 10 |}]
;;
