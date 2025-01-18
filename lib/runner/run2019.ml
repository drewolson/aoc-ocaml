let run day part input =
  (match day, part with
   | 1, 1 -> Stdio.printf "%i" @@ Year2019.Day01.part1 input
   | 1, 2 -> Stdio.printf "%i" @@ Year2019.Day01.part2 input
   | day, part -> failwith @@ Printf.sprintf "Unknown day %i and part %i" day part);
  Stdio.print_string "\n"
;;
