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
  Printf.printf "%i" @@ Year2022.Day01.part1 input;
  [%expect {| 24000 |}]
;;

let%expect_test "2022 day1 part2" =
  Printf.printf "%i" @@ Year2022.Day01.part2 input;
  [%expect {| 45000 |}]
;;
