let input = {|mjqjpqmgbljsphdztnvjfqwrcgsmlb|}

let%expect_test "2022 day6 part1" =
  Printf.printf "%i" @@ Year2022.Day06.part1 input;
  [%expect {| 7 |}]
;;

let%expect_test "2022 day7 part1" =
  Printf.printf "%i" @@ Year2022.Day06.part2 input;
  [%expect {| 19 |}]
;;
