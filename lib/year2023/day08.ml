module StrMap = Map.Make (String)
module P = Util.Parser
open P.Syntax

type dir =
  | R
  | L

type node = string * string

type input =
  { dirs : dir Sequence.t
  ; map : node StrMap.t
  }

let dir_p = P.choice [ R <$ P.char 'R'; L <$ P.char 'L' ]
let dirs_p = P.many_till dir_p P.end_of_line >>| Sequence.cycle_list_exn

let token_p =
  P.take_while (function
    | 'A' .. 'Z' -> true
    | '0' .. '9' -> true
    | _ -> false)
;;

let pair_p =
  let+ left = P.char '(' *> token_p <* P.string ", "
  and+ right = token_p <* P.char ')' in
  left, right
;;

let entry_p =
  let+ key = token_p <* P.string " = "
  and+ value = pair_p in
  key, value
;;

let map_p = P.sep_by1 P.end_of_line entry_p >>| StrMap.of_alist_exn

let input_p =
  let+ dirs = dirs_p <* P.end_of_line
  and+ map = map_p in
  { dirs; map }
;;

let solve { dirs; map } =
  let make_move (count, node) dir =
    if String.equal node "ZZZ"
    then Error count
    else (
      let l, r = Map.find_exn map node in
      match dir with
      | L -> Ok (count + 1, l)
      | R -> Ok (count + 1, r))
  in
  dirs |> Util.Sequence.fold_result_exn ~init:(0, "AAA") ~f:make_move
;;

let solve' { dirs; map } =
  let is_start s = Pcre.pmatch ~rex:(Pcre.regexp {|A\z|}) s in
  let is_end s = Pcre.pmatch ~rex:(Pcre.regexp {|Z\z|}) s in
  let starts = map |> Map.keys |> List.filter ~f:is_start in
  let make_move (count, steps, nodes) dir =
    let ends, rest = List.partition_tf nodes ~f:is_end in
    let steps' = if List.is_empty ends then steps else count :: steps in
    let nodes' =
      List.map rest ~f:(fun node ->
        let l, r = Map.find_exn map node in
        match dir with
        | L -> l
        | R -> r)
    in
    if List.is_empty nodes' then Error steps' else Ok (Z.add count Z.one, steps', nodes')
  in
  dirs
  |> Util.Sequence.fold_result_exn ~init:(Z.zero, [], starts) ~f:make_move
  |> List.fold ~init:Z.one ~f:Z.lcm
  |> Z.to_int
;;

let part1 input = input |> P.parse_exn input_p |> solve
let part2 input = input |> P.parse_exn input_p |> solve'
