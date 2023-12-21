let input =
  {|...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
|}
;;

let%expect_test "2023 day21 part1" =
  Printf.printf "%i" @@ Year2023.Day21.part1 6 input;
  [%expect {| 16 |}]
;;

let%expect_test "2023 day21 part2" =
  Printf.printf "%i" @@ Year2023.Day21.part2 10 input;
  [%expect {| 50 |}]
;;
