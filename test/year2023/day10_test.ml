let input = {|..F7.
.FJ|.
SJ.L7
|F--J
LJ...
|}

let input2 =
  {|...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
|}
;;

let input3 =
  {|.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
|}
;;

let input4 =
  {|FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
  |}
;;

let%expect_test "2023 day10 part1" =
  Printf.printf "%i" @@ Year2023.Day10.part1 input;
  [%expect {| 8 |}]
;;

let%expect_test "2023 day10 part2" =
  Printf.printf "%i" @@ Year2023.Day10.part2 false input2;
  [%expect {| 4 |}];
  Printf.printf "%i" @@ Year2023.Day10.part2 false input3;
  [%expect {| 8 |}];
  Printf.printf "%i" @@ Year2023.Day10.part2 false input4;
  [%expect {| 10 |}]
;;
