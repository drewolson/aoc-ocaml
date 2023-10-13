let input = {|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
|}

let%expect_test "2022 day1 part1" =
  let result = Year2022.Day01.part1 input in
  Printf.printf "%i" result;
  [%expect {| 24000 |}]
;;

let%expect_test "2022 day1 part2" =
  let result = Year2022.Day01.part2 input in
  Printf.printf "%i" result;
  [%expect {| 45000 |}]
;;
