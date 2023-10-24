let input = {|30373
25512
65332
33549
35390
|}

let%expect_test "2022 day8 part1" =
  let result = Year2022.Day08.part1 input in
  Printf.printf "%i" result;
  [%expect {| 21 |}]
;;

let%expect_test "2022 day8 part2" =
  let result = Year2022.Day08.part2 input in
  Printf.printf "%i" result;
  [%expect {| 8 |}]
;;
