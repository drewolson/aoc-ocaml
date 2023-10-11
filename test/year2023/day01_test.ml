let%expect_test "day1 part1" =
  let input = {|
        hello
      |} in

  let result = Year2023.Day01.part1 input in

  Printf.printf "%i" result;

  [%expect {| 1 |}]

let%expect_test "day1 part2" =
  let input = {|
        hello
      |} in

  let result = Year2023.Day01.part2 input in

  Printf.printf "%i" result;

  [%expect {| 2 |}]
