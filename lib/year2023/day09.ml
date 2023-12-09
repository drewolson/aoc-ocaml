let parse input =
  input
  |> String.split_lines
  |> List.map ~f:(fun l -> l |> String.split ~on:' ' |> List.map ~f:Int.of_string)
;;

let rec next_val pat =
  let rec pairs = function
    | a :: b :: t -> (a, b) :: pairs (b :: t)
    | _ -> []
  in
  if List.for_all pat ~f:(fun n -> n = 0)
  then 0
  else (
    let next = pat |> pairs |> List.map ~f:(fun (a, b) -> b - a) |> next_val in
    List.last_exn pat + next)
;;

let part1 input = input |> parse |> List.map ~f:next_val |> Util.List.sum_int

let part2 input =
  input |> parse |> List.map ~f:List.rev |> List.map ~f:next_val |> Util.List.sum_int
;;
