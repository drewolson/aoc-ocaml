module P = Util.Parser
open P.Syntax

type inst =
  | AddX of int
  | Noop
  | Wait

let addx_p =
  let%map i = P.string "addx " *> P.signed_integer in
  [ Wait; AddX i ]
;;

let inst_p = addx_p <|> ([ Noop ] <$ P.string "noop")

let insts_p =
  let%map insts = P.sep_by1 P.end_of_line inst_p in
  List.concat insts
;;

let execute (i, acc) = function
  | AddX n -> (i + 1, acc + n), (i, acc)
  | _ -> (i + 1, acc), (i, acc)
;;

let part1 input =
  input
  |> P.parse_exn insts_p
  |> List.folding_map ~init:(1, 1) ~f:execute
  |> Util.List.drop ~n:19
  |> List.chunks_of ~length:40
  |> List.map ~f:List.hd_exn
  |> List.sum (module Int) ~f:(fun (i, n) -> i * n)
;;

let to_line vals =
  vals
  |> List.zip_exn (List.init 40 ~f:Fn.id)
  |> List.map ~f:(fun (v, i) -> if v = i || v = i - 1 || v = i + 1 then "#" else ".")
  |> String.concat
;;

let part2 input =
  input
  |> P.parse_exn insts_p
  |> List.folding_map ~init:(0, 1) ~f:execute
  |> List.map ~f:snd
  |> List.chunks_of ~length:40
  |> List.map ~f:to_line
  |> String.concat ~sep:"\n"
;;
