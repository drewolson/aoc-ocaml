let input = {|broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
|}

let input2 = {|broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
|}

let%expect_test "2023 day20 part1" =
  Printf.printf "%i" @@ Year2023.Day20.part1 input;
  [%expect {| 32000000 |}];
  Printf.printf "%i" @@ Year2023.Day20.part1 input2;
  [%expect {| 11687500 |}]
;;
