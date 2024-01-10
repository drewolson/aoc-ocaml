let p = Printer.p

let run day part input b =
  match day, part with
  | 1, 1 -> p b "%i" @@ Year2022.Day01.part1 input
  | 1, 2 -> p b "%i" @@ Year2022.Day01.part2 input
  | 2, 1 -> p b "%i" @@ Year2022.Day02.part1 input
  | 2, 2 -> p b "%i" @@ Year2022.Day02.part2 input
  | 3, 1 -> p b "%i" @@ Year2022.Day03.part1 input
  | 3, 2 -> p b "%i" @@ Year2022.Day03.part2 input
  | 4, 1 -> p b "%i" @@ Year2022.Day04.part1 input
  | 4, 2 -> p b "%i" @@ Year2022.Day04.part2 input
  | 5, 1 -> p b "%s" @@ Year2022.Day05.part1 input
  | 5, 2 -> p b "%s" @@ Year2022.Day05.part2 input
  | 6, 1 -> p b "%i" @@ Year2022.Day06.part1 input
  | 6, 2 -> p b "%i" @@ Year2022.Day06.part2 input
  | 7, 1 -> p b "%i" @@ Year2022.Day07.part1 input
  | 7, 2 -> p b "%i" @@ Year2022.Day07.part2 input
  | 8, 1 -> p b "%i" @@ Year2022.Day08.part1 input
  | 8, 2 -> p b "%i" @@ Year2022.Day08.part2 input
  | 9, 1 -> p b "%i" @@ Year2022.Day09.part1 input
  | 9, 2 -> p b "%i" @@ Year2022.Day09.part2 input
  | 10, 1 -> p b "%i" @@ Year2022.Day10.part1 input
  | 10, 2 -> p b "%s" @@ Year2022.Day10.part2 input
  | 11, 1 -> p b "%s" @@ Year2022.Day11.part1 input
  | 11, 2 -> p b "%s" @@ Year2022.Day11.part1 input
  | day, part -> failwith @@ Printf.sprintf "Unknown day %i and part %i" day part
;;
