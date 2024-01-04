module IntMap = Map.Make (Int)
module P = Util.Parser
open P.Syntax

type inst =
  | Drop
  | Add of int

type op =
  { label : string
  ; hash : int
  ; inst : inst
  }

let parse input = input |> String.strip |> Pcre.split ~pat:","

let hash token =
  token
  |> String.to_list
  |> List.fold ~init:0 ~f:(fun acc c -> Int.rem ((Char.to_int c + acc) * 17) 256)
;;

let token_p =
  P.take_while (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let inst_p = P.choice [ Drop <$ P.char '-'; (P.char '=' *> P.integer >>| fun i -> Add i) ]

let op_p =
  let+ label = token_p
  and+ inst = inst_p in
  { label; hash = hash label; inst }
;;

let ops_p = P.sep_by1 (P.char ',') op_p

let execute map op =
  let replace items (k, v) =
    if List.Assoc.mem items ~equal:String.equal k
    then List.map items ~f:(fun (k', v') -> if String.equal k k' then k, v else k', v')
    else items @ [ k, v ]
  in
  match op.inst with
  | Add i ->
    Map.update map op.hash ~f:(function
      | None -> [ op.label, i ]
      | Some items -> replace items (op.label, i))
  | Drop ->
    Map.update map op.hash ~f:(function
      | None -> []
      | Some items -> List.Assoc.remove items ~equal:String.equal op.label)
;;

let focus_power ~key ~data sum =
  let power =
    data |> List.mapi ~f:(fun i (_, v) -> (key + 1) * (i + 1) * v) |> Util.List.sum_int
  in
  sum + power
;;

let part1 input = input |> parse |> List.sum (module Int) ~f:hash

let part2 input =
  input
  |> P.parse_exn ops_p
  |> List.fold ~init:IntMap.empty ~f:execute
  |> Map.fold ~init:0 ~f:focus_power
;;
