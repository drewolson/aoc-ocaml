let input =
  {|19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
|}
;;

let%expect_test "2023 day24 part1" =
  Printf.printf "%i" @@ Year2023.Day24.part1 Q.(~$7, ~$27) input;
  [%expect {| 2 |}]
;;

let%expect_test "2023 day24 part2" =
  Printf.printf "%i" @@ Year2023.Day24.part2 input;
  [%expect {| 2 |}]
;;
