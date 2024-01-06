module P = Util.Parser
open P.Syntax

type inst =
  | AddX of int
  | Noop

let addx_p =
  let+ i = P.string "addx " *> P.signed_integer in
  [ Noop; AddX i ]
;;

let inst_p = addx_p <|> ([ Noop ] <$ P.string "noop")
let insts_p = P.sep_by1 P.end_of_line inst_p >>| List.concat

let execute (i, acc) = function
  | AddX n -> (i + 1, acc + n), (i, acc)
  | Noop -> (i + 1, acc), (i, acc)
;;

let to_line vals =
  vals
  |> List.combine List.(0 --^ 40)
  |> List.map ~f:(fun (v, i) -> if abs (i - v) <= 1 then "#" else ".")
  |> String.concat ~sep:""
;;

let part1 input =
  input
  |> P.parse_exn insts_p
  |> List.fold_map ~init:(1, 1) ~f:execute
  |> snd
  |> List.drop 19
  |> List.chunks 40
  |> List.map ~f:List.hd
  |> List.fold_left ~init:0 ~f:(fun acc (i, n) -> acc + (i * n))
;;

let part2 input =
  input
  |> P.parse_exn insts_p
  |> List.fold_map ~init:(0, 1) ~f:execute
  |> snd
  |> List.map ~f:snd
  |> List.chunks 40
  |> List.map ~f:to_line
  |> String.concat ~sep:"\n"
;;
