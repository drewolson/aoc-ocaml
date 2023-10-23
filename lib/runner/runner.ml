let run2022 day part input =
  match day, part with
  | 1, 1 -> Printf.printf "%i\n" (Year2022.Day01.part1 input)
  | 1, 2 -> Printf.printf "%i\n" (Year2022.Day01.part2 input)
  | 2, 1 -> Printf.printf "%i\n" (Year2022.Day02.part1 input)
  | 2, 2 -> Printf.printf "%i\n" (Year2022.Day02.part2 input)
  | 3, 1 -> Printf.printf "%i\n" (Year2022.Day03.part1 input)
  | 3, 2 -> Printf.printf "%i\n" (Year2022.Day03.part2 input)
  | 4, 1 -> Printf.printf "%i\n" (Year2022.Day04.part1 input)
  | 4, 2 -> Printf.printf "%i\n" (Year2022.Day04.part2 input)
  | 5, 1 -> Printf.printf "%s\n" (Year2022.Day05.part1 input)
  | 5, 2 -> Printf.printf "%s\n" (Year2022.Day05.part2 input)
  | 6, 1 -> Printf.printf "%i\n" (Year2022.Day06.part1 input)
  | 6, 2 -> Printf.printf "%i\n" (Year2022.Day06.part2 input)
  | 7, 1 -> Printf.printf "%i\n" (Year2022.Day07.part1 input)
  | 7, 2 -> Printf.printf "%i\n" (Year2022.Day07.part2 input)
  | day, part -> failwith (Printf.sprintf "Unknown day %i and part %i" day part)
;;

let run2023 day part input =
  match day, part with
  | 1, 1 -> Printf.printf "%i\n" (Year2023.Day01.part1 input)
  | 1, 2 -> Printf.printf "%i\n" (Year2023.Day01.part2 input)
  | day, part -> failwith (Printf.sprintf "Unknown day %i and part %i" day part)
;;

let run year day part =
  let path = Printf.sprintf "data/%i/day%02i.txt" year day in
  let input = In_channel.read_all path in
  match year with
  | 2022 -> run2022 day part input
  | 2023 -> run2023 day part input
  | year -> failwith (Printf.sprintf "Unknown year: %i" year)
;;
