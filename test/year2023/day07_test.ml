let input = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
|}

let%expect_test "2023 day7 part1" =
  Printf.printf "%i" @@ Year2023.Day07.part1 input;
  [%expect {| 6440 |}]
;;

let%expect_test "2023 day7 part2" =
  Printf.printf "%i" @@ Year2023.Day07.part2 input;
  [%expect {| 5905 |}]
;;
