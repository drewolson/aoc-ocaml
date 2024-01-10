let p = Printer.p

let run day part input b =
  match day, part with
  | 1, 1 -> p b "%i" @@ Year2019.Day01.part1 input
  | 1, 2 -> p b "%i" @@ Year2019.Day01.part2 input
  | day, part -> failwith @@ Printf.sprintf "Unknown day %i and part %i" day part
;;
