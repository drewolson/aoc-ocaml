module P = Util.Parser
open P.Syntax

type color =
  | Red
  | Green
  | Blue
[@@deriving eq]

type draw =
  { count : int
  ; color : color
  }

type draws = draw list

type game =
  { id : int
  ; round : draws list
  }

let color_p =
  P.choice [ Red <$ P.string "red"; Green <$ P.string "green"; Blue <$ P.string "blue" ]
;;

let draw_p =
  let+ count = P.integer <* P.char ' '
  and+ color = color_p in
  { count; color }
;;

let draws_p = P.sep_by1 (P.string ", ") draw_p
let round_p = P.sep_by1 (P.string "; ") draws_p

let game_p =
  let+ id = P.string "Game " *> P.integer <* P.string ": "
  and+ round = round_p in
  { id; round }
;;

let games_p = P.sep_by1 P.end_of_line game_p

let count color g =
  let draw_count draws =
    List.fold_left draws ~init:0 ~f:(fun acc draw ->
      acc + if equal_color draw.color color then draw.count else 0)
  in
  g.round |> List.map ~f:draw_count |> Util.List.max_int
;;

let is_possible g = count Red g <= 12 && count Green g <= 13 && count Blue g <= 14
let power g = count Red g * count Green g * count Blue g

let part1 input =
  input
  |> P.parse_exn games_p
  |> List.filter ~f:is_possible
  |> List.fold_left ~init:0 ~f:(fun acc g -> acc + g.id)
;;

let part2 input =
  input |> P.parse_exn games_p |> List.fold_left ~init:0 ~f:(fun acc g -> acc + power g)
;;
