module P = Util.Parser
module IntMap = Map.Make (Int)
open P.Syntax

type move =
  { n : int
  ; from : int
  ; to' : int
  }

type crate = char

type instructions =
  { moves : move list
  ; crates : crate list IntMap.t
  }

let drop_line_p = P.skip_while (fun c -> not (Char.equal c '\n')) <* P.end_of_line
let no_crate_p = None <$ P.string "   "
let yes_crate_p = Option.some <$> P.char '[' *> P.any_char <* P.char ']'
let crate_p = no_crate_p <|> yes_crate_p
let crate_line_p = P.sep_by1 (P.char ' ') crate_p
let crate_lines_p = P.sep_by1 P.end_of_line crate_line_p <* P.end_of_line

let move_p =
  let+ n = P.string "move " *> P.integer
  and+ from = P.string " from " *> P.integer
  and+ to' = P.string " to " *> P.integer in
  { n; from; to' }
;;

let moves_p = P.sep_by1 P.end_of_line move_p

let pad_crates crates =
  let max = crates |> List.map ~f:List.length |> Util.List.max_int in
  List.map crates ~f:(fun l ->
    let pad = List.(length l --^ max) |> List.map ~f:(Fun.const None) in
    l @ pad)
;;

let to_stacks crates =
  crates
  |> pad_crates
  |> Util.List.transpose
  |> List.map ~f:List.keep_some
  |> List.mapi ~f:(fun i l -> i + 1, l)
  |> IntMap.of_list
;;

let instructions_p =
  let+ crate_lines = crate_lines_p <* drop_line_p <* drop_line_p
  and+ moves = moves_p in
  let crates = to_stacks crate_lines in
  { moves; crates }
;;

let move_crate f crates { n; from; to' } =
  let source = IntMap.find from crates in
  let dest = IntMap.find to' crates in
  let cs, source' = List.take_drop n source in
  crates |> IntMap.add from source' |> IntMap.add to' (f cs @ dest)
;;

let find_crates f input =
  let { moves; crates } = P.parse_exn instructions_p input in
  moves
  |> List.fold_left ~init:crates ~f:(move_crate f)
  |> IntMap.values
  |> List.of_iter
  |> List.map ~f:List.hd
  |> String.of_list
;;

let part1 input = find_crates List.rev input
let part2 input = find_crates Fun.id input
