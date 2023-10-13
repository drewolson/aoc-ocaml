let totals lines =
  let rec totals' all current = function
    | [] -> all
    | "" :: t -> totals' (current :: all) 0 t
    | h :: t -> totals' all (current + int_of_string h) t
  in
  totals' [] 0 lines
;;

let part1 input = input |> Util.String.lines |> totals |> Util.List.maximum min_int

let part2 input =
  input
  |> Util.String.lines
  |> totals
  |> Util.List.sort_desc
  |> Util.List.take 3
  |> Util.List.sum
;;
