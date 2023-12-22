let input =
  {|1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
|}
;;

let%expect_test "2023 day22 part1" =
  Printf.printf "%i" @@ Year2023.Day22.part1 input;
  [%expect {| 5 |}]
;;

let%expect_test "2023 day22 part2" =
  Printf.printf "%i" @@ Year2023.Day22.part2 input;
  [%expect {| 7 |}]
;;
