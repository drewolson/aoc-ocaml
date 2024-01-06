let totals input =
  input
  |> Pcre.split ~rex:(Pcre.regexp "\n\n")
  |> List.map ~f:(fun ls ->
    ls |> String.lines |> List.map ~f:Int.of_string_exn |> List.fold_left ~init:0 ~f:( + ))
;;

let part1 input = input |> totals |> Util.List.max_int

let part2 input =
  input
  |> totals
  |> List.sort ~cmp:(fun x y -> -compare x y)
  |> List.take 3
  |> List.fold_left ~init:0 ~f:( + )
;;
