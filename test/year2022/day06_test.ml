let input = {|mjqjpqmgbljsphdztnvjfqwrcgsmlb|}

let%expect_test "2022 day6 part1" =
  let result = Year2022.Day06.part1 input in
  Printf.printf "%i" result;
  [%expect {| 7 |}]
;;

let%expect_test "2022 day7 part1" =
  let result = Year2022.Day06.part2 input in
  Printf.printf "%i" result;
  [%expect {| 19 |}]
;;
