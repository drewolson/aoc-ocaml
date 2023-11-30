let input =
  {|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
|}
;;

let%expect_test "2022 day3 part1" =
  Printf.printf "%i" @@ Year2022.Day03.part1 input;
  [%expect {| 157 |}]
;;

let%expect_test "2022 day3 part2" =
  Printf.printf "%i" @@ Year2022.Day03.part2 input;
  [%expect {| 70 |}]
;;
