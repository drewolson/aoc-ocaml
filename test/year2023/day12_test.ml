let input =
  {|???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
|}
;;

let%expect_test "2023 day12 part1" =
  Printf.printf "%i" @@ Year2023.Day12.part1 input;
  [%expect {| 21 |}]
;;

let%expect_test "2023 day12 part2" =
  Printf.printf "%i" @@ Year2023.Day12.part2 input;
  [%expect {| 525152 |}]
;;
