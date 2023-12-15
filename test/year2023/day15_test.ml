let input = {|rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
|}

let%expect_test "2023 day15 part1" =
  Printf.printf "%i" @@ Year2023.Day15.part1 input;
  [%expect {| 1320 |}]
;;

let%expect_test "2023 day15 part2" =
  Printf.printf "%i" @@ Year2023.Day15.part2 input;
  [%expect {| 145 |}]
;;
