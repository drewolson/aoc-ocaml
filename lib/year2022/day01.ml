let totals input =
  input
  |> Pcre.split ~rex:(Pcre.regexp "\n\n")
  |> List.map ~f:(fun ls ->
    ls |> String.split_lines |> List.map ~f:Int.of_string |> Util.List.sum_int)
;;

let part1 input = input |> totals |> Util.List.max_int

let part2 input =
  input
  |> totals
  |> List.sort ~compare:(fun x y -> -compare x y)
  |> Util.List.take ~n:3
  |> Util.List.sum_int
;;
