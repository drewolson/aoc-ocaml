module P = Util.Parser
open P.Syntax

type race =
  { time : int
  ; distance : int
  }

let races_p =
  let%map times =
    P.string "Time:" *> P.spaces *> P.sep_by1 P.spaces P.integer <* P.end_of_line
  and distances = P.string "Distance:" *> P.spaces *> P.sep_by1 P.spaces P.integer in
  [ times; distances ]
  |> List.transpose_exn
  |> List.filter_map ~f:(function
    | [ time; distance ] -> Some { time; distance }
    | _ -> None)
;;

let race_p =
  let%map times =
    P.string "Time:" *> P.spaces *> P.sep_by1 P.spaces P.digits <* P.end_of_line
  and distances = P.string "Distance:" *> P.spaces *> P.sep_by1 P.spaces P.digits in
  { time = times |> String.concat |> Int.of_string
  ; distance = distances |> String.concat |> Int.of_string
  }
;;

let beat_count { time; distance } =
  Sequence.range 1 time
  |> Sequence.map ~f:(fun hold -> hold * (time - hold))
  |> Sequence.filter ~f:(fun n -> n > distance)
  |> Sequence.length
;;

let part1 input =
  input |> P.parse_exn races_p |> List.map ~f:beat_count |> List.fold ~init:1 ~f:( * )
;;

let part2 input = input |> P.parse_exn race_p |> beat_count
