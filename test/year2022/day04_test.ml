let input = {|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
|}

let%expect_test "2022 day4 part1" =
  let result = Year2022.Day04.part1 input in
  Printf.printf "%i" result;
  [%expect {| 2 |}]
;;

let%expect_test "2022 day4 part2" =
  let result = Year2022.Day04.part2 input in
  Printf.printf "%i" result;
  [%expect {| 4 |}]
;;
