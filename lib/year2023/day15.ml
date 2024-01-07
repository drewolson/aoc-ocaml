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

let parse input = input |> String.trim |> Pcre.split ~pat:","

let hash token =
  token
  |> String.to_list
  |> List.fold_left ~init:0 ~f:(fun acc c -> Int.rem ((Char.to_int c + acc) * 17) 256)
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
    if List.Assoc.mem ~eq:String.equal k items
    then List.map items ~f:(fun (k', v') -> if String.equal k k' then k, v else k', v')
    else items @ [ k, v ]
  in
  match op.inst with
  | Add i ->
    IntMap.update
      op.hash
      (function
        | None -> Some [ op.label, i ]
        | Some items -> Some (replace items (op.label, i)))
      map
  | Drop ->
    IntMap.update
      op.hash
      (function
        | None -> Some []
        | Some items -> Some (List.Assoc.remove ~eq:String.equal op.label items))
      map
;;

let focus_power key data sum =
  let power =
    data |> List.mapi ~f:(fun i (_, v) -> (key + 1) * (i + 1) * v) |> Util.List.sum_int
  in
  sum + power
;;

let part1 input = input |> parse |> List.fold_left ~init:0 ~f:(fun acc t -> acc + hash t)

let part2 input =
  let map = input |> P.parse_exn ops_p |> List.fold_left ~init:IntMap.empty ~f:execute in
  IntMap.fold focus_power map 0
;;
