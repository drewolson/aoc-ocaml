let run day part input =
  (match day, part with
   | 1, 1 -> Stdio.printf "%i" @@ Year2023.Day01.part1 input
   | 1, 2 -> Stdio.printf "%i" @@ Year2023.Day01.part2 input
   | 2, 1 -> Stdio.printf "%i" @@ Year2023.Day02.part1 input
   | 2, 2 -> Stdio.printf "%i" @@ Year2023.Day02.part2 input
   | 3, 1 -> Stdio.printf "%i" @@ Year2023.Day03.part1 input
   | 3, 2 -> Stdio.printf "%i" @@ Year2023.Day03.part2 input
   | 4, 1 -> Stdio.printf "%i" @@ Year2023.Day04.part1 input
   | 4, 2 -> Stdio.printf "%i" @@ Year2023.Day04.part2 input
   | 5, 1 -> Stdio.printf "%i" @@ Year2023.Day05.part1 input
   | 5, 2 -> Stdio.printf "%i" @@ Year2023.Day05.part2 input
   | 6, 1 -> Stdio.printf "%i" @@ Year2023.Day06.part1 input
   | 6, 2 -> Stdio.printf "%i" @@ Year2023.Day06.part2 input
   | 7, 1 -> Stdio.printf "%i" @@ Year2023.Day07.part1 input
   | 7, 2 -> Stdio.printf "%i" @@ Year2023.Day07.part2 input
   | 8, 1 -> Stdio.printf "%i" @@ Year2023.Day08.part1 input
   | 8, 2 -> Stdio.printf "%i" @@ Year2023.Day08.part2 input
   | 9, 1 -> Stdio.printf "%i" @@ Year2023.Day09.part1 input
   | 9, 2 -> Stdio.printf "%i" @@ Year2023.Day09.part2 input
   | 10, 1 -> Stdio.printf "%i" @@ Year2023.Day10.part1 input
   | 10, 2 -> Stdio.printf "%i" @@ Year2023.Day10.part2 true input
   | 11, 1 -> Stdio.printf "%i" @@ Year2023.Day11.part1 input
   | 11, 2 -> Stdio.printf "%i" @@ Year2023.Day11.part2 input
   | 12, 1 -> Stdio.printf "%i" @@ Year2023.Day12.part1 input
   | 12, 2 -> Stdio.printf "%i" @@ Year2023.Day12.part2 input
   | 13, 1 -> Stdio.printf "%i" @@ Year2023.Day13.part1 input
   | 13, 2 -> Stdio.printf "%i" @@ Year2023.Day13.part2 input
   | 14, 1 -> Stdio.printf "%i" @@ Year2023.Day14.part1 input
   | 14, 2 -> Stdio.printf "%i" @@ Year2023.Day14.part2 input
   | 15, 1 -> Stdio.printf "%i" @@ Year2023.Day15.part1 input
   | 15, 2 -> Stdio.printf "%i" @@ Year2023.Day15.part2 input
   | 16, 1 -> Stdio.printf "%i" @@ Year2023.Day16.part1 input
   | 16, 2 -> Stdio.printf "%i" @@ Year2023.Day16.part2 input
   | 17, 1 -> Stdio.printf "%i" @@ Year2023.Day17.part1 input
   | 17, 2 -> Stdio.printf "%i" @@ Year2023.Day17.part2 input
   | 18, 1 -> Stdio.printf "%i" @@ Year2023.Day18.part1 input
   | 18, 2 -> Stdio.printf "%i" @@ Year2023.Day18.part2 input
   | 19, 1 -> Stdio.printf "%i" @@ Year2023.Day19.part1 input
   | 19, 2 -> Stdio.printf "%i" @@ Year2023.Day19.part2 input
   | 20, 1 -> Stdio.printf "%i" @@ Year2023.Day20.part1 input
   | 20, 2 -> Stdio.printf "%i" @@ Year2023.Day20.part2 input
   | 21, 1 -> Stdio.printf "%i" @@ Year2023.Day21.part1 64 input
   | 21, 2 -> Stdio.printf "%i" @@ Year2023.Day21.part2 input
   | 22, 1 -> Stdio.printf "%i" @@ Year2023.Day22.part1 input
   | 22, 2 -> Stdio.printf "%i" @@ Year2023.Day22.part2 input
   | 23, 1 -> Stdio.printf "%i" @@ Year2023.Day23.part1 input
   | 23, 2 -> Stdio.printf "%i" @@ Year2023.Day23.part2 input
   | 24, 1 ->
     Stdio.printf "%i"
     @@ Year2023.Day24.part1 Q.(~$200000000000000, ~$400000000000000) input
   | 24, 2 -> Stdio.printf "%i" @@ Year2023.Day24.part2 input
   | 25, 1 -> Stdio.printf "%i" @@ Year2023.Day25.part1 input
   | day, part -> failwith @@ Printf.sprintf "Unknown day %i and part %i" day part);
  Stdio.print_string "\n"
;;
