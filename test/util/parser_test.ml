module P = Util.Parser

let%expect_test "signed_integer works with positive signs" =
  Printf.printf "%i" @@ P.parse_all_exn P.signed_integer "+10";
  [%expect {| 10 |}]
;;

let%expect_test "signed_integer works with negative signs" =
  Printf.printf "%i" @@ P.parse_all_exn P.signed_integer "-10";
  [%expect {| -10 |}]
;;

let%expect_test "signed_integer works with no signs" =
  Printf.printf "%i" @@ P.parse_all_exn P.signed_integer "10";
  [%expect {| 10 |}]
;;
