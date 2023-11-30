let input = {|30373
25512
65332
33549
35390
|}

let%expect_test "2022 day8 part1" =
  Printf.printf "%i" @@ Year2022.Day08.part1 input;
  [%expect {| 21 |}]
;;

let%expect_test "2022 day8 part2" =
  Printf.printf "%i" @@ Year2022.Day08.part2 input;
  [%expect {| 8 |}]
;;
