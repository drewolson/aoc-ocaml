let input =
  {|...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
|}
;;

let%expect_test "2023 day11 part1" =
  Printf.printf "%i" @@ Year2023.Day11.part1 input;
  [%expect {| 374 |}]
;;

let%expect_test "2023 day11 part2" =
  Printf.printf "%i" @@ Year2023.Day11.part2 input;
  [%expect {| 82000210 |}]
;;
