let totals input =
  input
  |> Re.Str.split (Re.Str.regexp "\n\n")
  |> List.map ~f:(fun ls ->
    ls |> String.split_lines |> List.map ~f:Int.of_string |> Util.List.sum_int)
;;

let part1 input = input |> totals |> Util.List.max_int

let part2 input =
  input
  |> totals
  |> List.sort ~compare:(fun x y -> -compare x y)
  |> Util.List.take 3
  |> Util.List.sum_int
;;
