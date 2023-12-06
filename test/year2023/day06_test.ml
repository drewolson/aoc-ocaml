let input = {|Time:      7  15   30
Distance:  9  40  200
|}

let%expect_test "2023 day6 part1" =
  Printf.printf "%i" @@ Year2023.Day06.part1 input;
  [%expect {| 288 |}]
;;

let%expect_test "2023 day6 part2" =
  Printf.printf "%i" @@ Year2023.Day06.part2 input;
  [%expect {| 71503 |}]
;;
