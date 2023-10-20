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
  let result = Year2022.Day03.part1 input in
  Printf.printf "%i" result;
  [%expect {| 157 |}]
;;

let%expect_test "2022 day3 part2" =
  let result = Year2022.Day03.part2 input in
  Printf.printf "%i" result;
  [%expect {| 70 |}]
;;
