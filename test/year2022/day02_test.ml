let input = {|A Y
B X
C Z
|}

let%expect_test "2022 day2 part1" =
  let result = Year2022.Day02.part1 input in
  Printf.printf "%i" result;
  [%expect {| 15 |}]
;;

let%expect_test "2022 day2 part2" =
  let result = Year2022.Day02.part2 input in
  Printf.printf "%i" result;
  [%expect {| 12 |}]
;;
