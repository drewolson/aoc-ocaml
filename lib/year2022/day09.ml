module P = Util.Parser
open P.Syntax

module Coord = struct
  type t = int * int [@@deriving sexp, compare]
end

module CoordSet = Set.Make (Coord)

let dir_p =
  P.choice
    [ (1, 0) <$ P.char 'R'
    ; (-1, 0) <$ P.char 'L'
    ; (0, 1) <$ P.char 'U'
    ; (0, -1) <$ P.char 'D'
    ]
;;

let step_p =
  let%map dir = dir_p <* P.char ' '
  and num = P.integer in
  dir, num
;;

let steps_p = P.sep_by1 P.end_of_line step_p

let parse_coords input =
  input
  |> P.parse_exn steps_p
  |> List.concat_map ~f:(fun (coord, n) -> Util.List.replicate ~n coord)
;;

let take_steps n steps =
  let signum n = if n < 0 then -1 else if n > 0 then 1 else 0 in
  let move (x, y) (dx, dy) = x + dx, y + dy in
  let move_tail (hx, hy) (tx, ty) =
    if abs (hx - tx) >= 2 || abs (hy - ty) >= 2
    then tx + signum (hx - tx), ty + signum (hy - ty)
    else tx, ty
  in
  let take_step (set, h, ts) d =
    let h' = move h d in
    let ts' =
      List.folding_map ts ~init:h' ~f:(fun acc a ->
        let a' = move_tail acc a in
        a', a')
    in
    let set' = Set.add set (List.last_exn ts') in
    set', h', ts'
  in
  steps
  |> List.fold
       ~init:(CoordSet.singleton (0, 0), (0, 0), Util.List.replicate ~n (0, 0))
       ~f:take_step
  |> fun (x, _, _) -> x
;;

let part1 input = input |> parse_coords |> take_steps 1 |> Set.length
let part2 input = input |> parse_coords |> take_steps 9 |> Set.length
