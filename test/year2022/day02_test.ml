let input = {|A Y
B X
C Z
|}

let%expect_test "2022 day2 part1" =
  Printf.printf "%i" @@ Year2022.Day02.part1 input;
  [%expect {| 15 |}]
;;

let%expect_test "2022 day2 part2" =
  Printf.printf "%i" @@ Year2022.Day02.part2 input;
  [%expect {| 12 |}]
;;
