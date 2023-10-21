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
  let result = Year2022.Day05.part1 input in
  Printf.printf "%s" result;
  [%expect {| CMZ |}]
;;

let%expect_test "2022 day5 part2" =
  let result = Year2022.Day05.part2 input in
  Printf.printf "%s" result;
  [%expect {| MCD |}]
;;
