let input =
  {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
|}
;;

let%expect_test "2023 day3 part1" =
  Printf.printf "%i" @@ Year2023.Day03.part1 input;
  [%expect {| 4361 |}]
;;

(* [%expect {| 4361 |}] *)

let%expect_test "2023 day3 part2" =
  Printf.printf "%i" @@ Year2023.Day03.part2 input;
  [%expect {| 467835 |}]
;;
