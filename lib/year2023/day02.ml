module P = Util.Parser
open P.Syntax

type color =
  | Red
  | Green
  | Blue
[@@deriving equal]

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
  let%map count = P.integer <* P.char ' '
  and color = color_p in
  { count; color }
;;

let draws_p = P.sep_by1 (P.string ", ") draw_p
let round_p = P.sep_by1 (P.string "; ") draws_p

let game_p =
  let%map id = P.string "Game " *> P.integer <* P.string ": "
  and round = round_p in
  { id; round }
;;

let games_p = P.sep_by1 P.end_of_line game_p

let count color g =
  let draw_count draws =
    List.sum
      (module Int)
      draws
      ~f:(fun draw -> if equal_color draw.color color then draw.count else 0)
  in
  List.fold g.round ~init:0 ~f:(fun acc draws -> max acc (draw_count draws))
;;

let is_possible g = count Red g <= 12 && count Green g <= 13 && count Blue g <= 14
let power g = count Red g * count Green g * count Blue g

let part1 input =
  input
  |> P.parse_exn games_p
  |> List.filter ~f:is_possible
  |> List.sum (module Int) ~f:(fun g -> g.id)
;;

let part2 input = input |> P.parse_exn games_p |> List.sum (module Int) ~f:power
