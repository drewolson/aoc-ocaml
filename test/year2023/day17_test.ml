let input =
  {|2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
|}
;;

let input2 = {|111111111111
999999999991
999999999991
999999999991
999999999991
  |}

let%expect_test "2023 day17 part1" =
  Printf.printf "%i" @@ Year2023.Day17.part1 input;
  [%expect {| 102 |}]
;;

let%expect_test "2023 day17 part2" =
  Printf.printf "%i" @@ Year2023.Day17.part2 input;
  [%expect {| 94 |}];
  Printf.printf "%i" @@ Year2023.Day17.part2 input2;
  [%expect {| 71 |}]
;;