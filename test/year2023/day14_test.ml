let input =
  {|O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
|}
;;

let%expect_test "2023 day14 part1" =
  Printf.printf "%i" @@ Year2023.Day14.part1 input;
  [%expect {| 136 |}]
;;

let%expect_test "2023 day14 part2" =
  Printf.printf "%i" @@ Year2023.Day14.part2 input;
  [%expect {| 64 |}]
;;
