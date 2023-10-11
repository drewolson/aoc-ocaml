let run2023 day part input =
  match (day, part) with
  | 1, 1 -> Printf.printf "%i" (Year2023.Day01.part1 input)
  | 1, 2 -> Printf.printf "%i" (Year2023.Day01.part2 input)
  | day, part -> failwith (Printf.sprintf "Unknown day %i and part %i" day part)

let run year day part =
  let path = Printf.sprintf "data/%i/day%02i.txt" year day in
  let input = In_channel.with_open_bin path In_channel.input_all in

  match year with
  | 2023 -> run2023 day part input
  | year -> failwith (Printf.sprintf "Unknown year: %i" year)
