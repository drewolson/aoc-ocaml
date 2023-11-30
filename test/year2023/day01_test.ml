let input = {|
hello
|}

let%expect_test "2023 day1 part1" =
  Printf.printf "%i" @@ Year2023.Day01.part1 input;
  [%expect {| 1 |}]
;;

let%expect_test "2023 day1 part2" =
  Printf.printf "%i" @@ Year2023.Day01.part2 input;
  [%expect {| 2 |}]
;;
