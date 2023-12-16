let input =
  {|.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
|}
;;

let%expect_test "2023 day16 part1" =
  Printf.printf "%i" @@ Year2023.Day16.part1 input;
  [%expect {| 46 |}]
;;

let%expect_test "2023 day16 part2" =
  Printf.printf "%i" @@ Year2023.Day16.part2 input;
  [%expect {| 51 |}]
;;
