let input = {|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
|}

let input2 = {|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
|}

let%expect_test "2022 day9 part1" =
  let result = Year2022.Day09.part1 input in
  Printf.printf "%i" result;
  [%expect {| 13 |}]
;;

let%expect_test "2022 day9 part2" =
  let result = Year2022.Day09.part2 input2 in
  Printf.printf "%i" result;
  [%expect {| 36 |}]
;;
