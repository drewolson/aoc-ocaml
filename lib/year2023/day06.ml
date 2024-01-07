module P = Util.Parser
open P.Syntax

type race =
  { time : int
  ; distance : int
  }

let data_p p =
  let+ times = P.string "Time:" *> P.spaces *> P.sep_by1 P.spaces p <* P.end_of_line
  and+ distances = P.string "Distance:" *> P.spaces *> P.sep_by1 P.spaces p in
  times, distances
;;

let races_p =
  let+ times, distances = data_p P.integer in
  [ times; distances ]
  |> Util.List.transpose
  |> List.filter_map ~f:(function
    | [ time; distance ] -> Some { time; distance }
    | _ -> None)
;;

let race_p =
  let+ times, distances = data_p P.digits in
  { time = times |> String.concat ~sep:"" |> Int.of_string_exn
  ; distance = distances |> String.concat ~sep:"" |> Int.of_string_exn
  }
;;

let beat_count { time; distance } =
  Seq.(1 --^ time)
  |> Seq.map (fun hold -> hold * (time - hold))
  |> Seq.filter (fun n -> n > distance)
  |> Seq.length
;;

let part1 input =
  input
  |> P.parse_exn races_p
  |> List.map ~f:beat_count
  |> List.fold_left ~init:1 ~f:( * )
;;

let part2 input = input |> P.parse_exn race_p |> beat_count
