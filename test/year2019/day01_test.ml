let input = {|12
14
1969
100756
|}

let%expect_test "2019 day1 part1" =
  Printf.printf "%i" @@ Year2019.Day01.part1 input;
  [%expect {| 34241 |}]
;;

let%expect_test "2019 day1 part2" =
  Printf.printf "%i" @@ Year2019.Day01.part2 input;
  [%expect {| 51316 |}]
;;
