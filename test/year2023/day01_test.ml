let input = {|
hello
|}

let%expect_test "2023 day1 part1" =
  let result = Year2023.Day01.part1 input in
  Printf.printf "%i" result;
  [%expect {| 1 |}]
;;

let%expect_test "2023 day1 part2" =
  let result = Year2023.Day01.part2 input in
  Printf.printf "%i" result;
  [%expect {| 2 |}]
;;
