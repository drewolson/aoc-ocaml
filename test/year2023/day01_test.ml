let input = {|1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|}

let input2 =
  {|two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
|}
;;

let%expect_test "2023 day1 part1" =
  Printf.printf "%i" @@ Year2023.Day01.part1 input;
  [%expect {| 142 |}]
;;

let%expect_test "2023 day1 part2" =
  Printf.printf "%i" @@ Year2023.Day01.part2 input2;
  [%expect {| 281 |}]
;;
