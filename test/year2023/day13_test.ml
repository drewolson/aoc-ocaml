let input =
  {|#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
|}
;;

let%expect_test "2023 day13 part1" =
  Printf.printf "%i" @@ Year2023.Day13.part1 input;
  [%expect {| 405 |}]
;;

let%expect_test "2023 day13 part2" =
  Printf.printf "%i" @@ Year2023.Day13.part2 input;
  [%expect {| 400 |}]
;;
