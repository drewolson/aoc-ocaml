let input1 =
  {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
|}
;;

let input2 = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
|}

let input3 =
  {|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
|}
;;

let%expect_test "2023 day8 part1" =
  Printf.printf "%i" @@ Year2023.Day08.part1 input1;
  [%expect {| 2 |}];
  Printf.printf "%i" @@ Year2023.Day08.part1 input2;
  [%expect {| 6 |}]
;;

let%expect_test "2023 day8 part2" =
  Printf.printf "%i" @@ Year2023.Day08.part2 input3;
  [%expect {| 6 |}]
;;
