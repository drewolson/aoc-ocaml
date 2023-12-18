module P = Util.Parser
open P.Syntax

type dir =
  | U
  | D
  | L
  | R

module Coord = struct
  type t = int * int
end

type command =
  { dir : dir
  ; length : int
  ; color : string
  }

let dir_p =
  P.choice [ U <$ P.char 'U'; D <$ P.char 'D'; L <$ P.char 'L'; R <$ P.char 'R' ]
;;

let hex_p =
  P.take_while (function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let command_p =
  let%map dir = dir_p <* P.char ' '
  and length = P.integer <* P.char ' '
  and color = P.string "(#" *> hex_p <* P.char ')' in
  { dir; length; color }
;;

let commands_p = P.sep_by1 P.end_of_line command_p

let dig (x, y) command =
  let next =
    match command.dir with
    | U -> x, y - command.length
    | D -> x, y + command.length
    | L -> x - command.length, y
    | R -> x + command.length, y
  in
  next, (x, y)
;;

let shoelace points =
  let pairs = List.zip_exn points (List.drop points 1 @ [ List.hd_exn points ]) in
  let sum =
    List.sum (module Int) pairs ~f:(fun ((x1, y1), (x2, y2)) -> (x1 * y2) - (x2 * y1))
  in
  abs (sum / 2)
;;

let area commands =
  let border = List.sum (module Int) commands ~f:(fun c -> c.length) in
  let points = commands |> List.folding_map ~init:(0, 0) ~f:dig in
  shoelace points + (border / 2) + 1
;;

let from_hex command =
  let length_h, dir_h = List.split_n (String.to_list command.color) 5 in
  let length = Int.of_string ("0x" ^ String.of_list length_h) in
  let dir =
    match Int.of_string ("0x" ^ String.of_list dir_h) with
    | 0 -> R
    | 1 -> D
    | 2 -> L
    | _ -> U
  in
  { length; dir; color = "" }
;;

let part1 input = input |> P.parse_exn commands_p |> area
let part2 input = input |> P.parse_exn commands_p |> List.map ~f:from_hex |> area
