let input =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|}
;;

let%expect_test "2023 day5 part1" =
  Printf.printf "%i" @@ Year2023.Day05.part1 input;
  [%expect {| 35 |}]
;;

let%expect_test "2023 day5 part2" =
  Printf.printf "%i" @@ Year2023.Day05.part2 input;
  [%expect {| 46 |}]
;;
