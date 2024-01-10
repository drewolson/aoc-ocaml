let parse input = input |> String.split_lines |> List.map ~f:Int.of_string

let fuel mass =
  let rec aux total m =
    match (m / 3) - 2 with
    | f when f <= 0 -> total
    | f -> aux (total + f) f
  in
  aux 0 mass
;;

let part1 input = input |> parse |> List.sum (module Int) ~f:(fun m -> (m / 3) - 2)
let part2 input = input |> parse |> List.sum (module Int) ~f:fuel
