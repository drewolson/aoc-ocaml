let input =
  {|    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|}
;;

let%expect_test "2022 day5 part1" =
  Printf.printf "%s" @@ Year2022.Day05.part1 input;
  [%expect {| CMZ |}]
;;

let%expect_test "2022 day5 part2" =
  Printf.printf "%s" @@ Year2022.Day05.part2 input;
  [%expect {| MCD |}]
;;
