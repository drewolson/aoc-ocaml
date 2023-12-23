let p b f =
  if b
  then ignore
  else
    fun s ->
    Printf.printf f s;
    Stdio.print_string "\n"
;;

let run2023 day part input b =
  match day, part with
  | 1, 1 -> p b "%i" @@ Year2023.Day01.part1 input
  | 1, 2 -> p b "%i" @@ Year2023.Day01.part2 input
  | 2, 1 -> p b "%i" @@ Year2023.Day02.part1 input
  | 2, 2 -> p b "%i" @@ Year2023.Day02.part2 input
  | 3, 1 -> p b "%i" @@ Year2023.Day03.part1 input
  | 3, 2 -> p b "%i" @@ Year2023.Day03.part2 input
  | 4, 1 -> p b "%i" @@ Year2023.Day04.part1 input
  | 4, 2 -> p b "%i" @@ Year2023.Day04.part2 input
  | 5, 1 -> p b "%i" @@ Year2023.Day05.part1 input
  | 5, 2 -> p b "%i" @@ Year2023.Day05.part2 input
  | 6, 1 -> p b "%i" @@ Year2023.Day06.part1 input
  | 6, 2 -> p b "%i" @@ Year2023.Day06.part2 input
  | 7, 1 -> p b "%i" @@ Year2023.Day07.part1 input
  | 7, 2 -> p b "%i" @@ Year2023.Day07.part2 input
  | 8, 1 -> p b "%i" @@ Year2023.Day08.part1 input
  | 8, 2 -> p b "%i" @@ Year2023.Day08.part2 input
  | 9, 1 -> p b "%i" @@ Year2023.Day09.part1 input
  | 9, 2 -> p b "%i" @@ Year2023.Day09.part2 input
  | 10, 1 -> p b "%i" @@ Year2023.Day10.part1 input
  | 10, 2 -> p b "%i" @@ Year2023.Day10.part2 true input
  | 11, 1 -> p b "%i" @@ Year2023.Day11.part1 input
  | 11, 2 -> p b "%i" @@ Year2023.Day11.part2 input
  | 12, 1 -> p b "%i" @@ Year2023.Day12.part1 input
  | 12, 2 -> p b "%i" @@ Year2023.Day12.part2 input
  | 13, 1 -> p b "%i" @@ Year2023.Day13.part1 input
  | 13, 2 -> p b "%i" @@ Year2023.Day13.part2 input
  | 14, 1 -> p b "%i" @@ Year2023.Day14.part1 input
  | 14, 2 -> p b "%i" @@ Year2023.Day14.part2 input
  | 15, 1 -> p b "%i" @@ Year2023.Day15.part1 input
  | 15, 2 -> p b "%i" @@ Year2023.Day15.part2 input
  | 16, 1 -> p b "%i" @@ Year2023.Day16.part1 input
  | 16, 2 -> p b "%i" @@ Year2023.Day16.part2 input
  | 17, 1 -> p b "%i" @@ Year2023.Day17.part1 input
  | 17, 2 -> p b "%i" @@ Year2023.Day17.part2 input
  | 18, 1 -> p b "%i" @@ Year2023.Day18.part1 input
  | 18, 2 -> p b "%i" @@ Year2023.Day18.part2 input
  | 19, 1 -> p b "%i" @@ Year2023.Day19.part1 input
  | 19, 2 -> p b "%i" @@ Year2023.Day19.part2 input
  | 20, 1 -> p b "%i" @@ Year2023.Day20.part1 input
  | 20, 2 -> p b "%i" @@ Year2023.Day20.part2 input
  | 21, 1 -> p b "%i" @@ Year2023.Day21.part1 64 input
  | 21, 2 -> p b "%i" @@ Year2023.Day21.part2 input
  | 22, 1 -> p b "%i" @@ Year2023.Day22.part1 input
  | 22, 2 -> p b "%i" @@ Year2023.Day22.part2 input
  | 23, 1 -> p b "%i" @@ Year2023.Day23.part1 input
  | 23, 2 -> p b "%i" @@ Year2023.Day23.part2 input
  | day, part -> failwith @@ Printf.sprintf "Unknown day %i and part %i" day part
;;

let run2022 day part input b =
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

let run year day part bench =
  let path = Printf.sprintf "data/%i/day%02i.txt" year day in
  let input = In_channel.read_all path in
  let maybe_bench ~f =
    let open Core_bench in
    let name = Printf.sprintf "Year %i, Day %i, Part %i" year day part in
    if bench then Bench.bench [ Bench.Test.create ~name f ] else f ()
  in
  maybe_bench ~f:(fun _ ->
    match year with
    | 2022 -> run2022 day part input bench
    | 2023 -> run2023 day part input bench
    | year -> failwith @@ Printf.sprintf "Unknown year: %i" year)
;;
