module P = Util.Parser
module A = Angstrom
module IntMap = Map.Make (Int)
open A.Let_syntax

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

let ( >>| ), ( *> ), ( <* ), ( <|> ), ( <$> ) =
  A.(( >>| ), ( *> ), ( <* ), ( <|> ), ( <$> ))
;;

let dropLineP = A.skip_while (fun c -> not (Char.equal c '\n')) <* A.end_of_line
let ( $> ) p a = p >>| const a
let ( <$ ) a p = p >>| const a
let noCrateP = None <$ A.string "   "
let yesCrateP = Option.some <$> A.char '[' *> A.any_char <* A.char ']'
let crateP = noCrateP <|> yesCrateP
let crateLineP = A.sep_by1 (A.char ' ') crateP
let crateLinesP = A.sep_by1 A.end_of_line crateLineP <* A.end_of_line

let moveP =
  let%map n = A.string "move " *> P.integerP
  and from = A.string " from " *> P.integerP
  and to' = A.string " to " *> P.integerP in
  { n; from; to' }
;;

let movesP = A.sep_by1 A.end_of_line moveP

let pad_crates crates =
  let max = crates |> List.map ~f:List.length |> Util.List.max_int in
  List.map crates ~f:(fun l ->
    let pad = List.init (max - List.length l) ~f:(const None) in
    List.append l pad)
;;

let to_stacks crates =
  crates
  |> pad_crates
  |> List.transpose_exn
  |> List.map ~f:List.filter_opt
  |> List.mapi ~f:(fun i l -> i + 1, l)
  |> IntMap.of_alist_exn
;;

let instructionsP =
  let%map crateLines = crateLinesP <* dropLineP <* dropLineP
  and moves = movesP in
  let crates = to_stacks crateLines in
  { moves; crates }
;;

let move_crate f crates { n; from; to' } =
  let source = Map.find_exn crates from in
  let dest = Map.find_exn crates to' in
  let cs, source' = List.split_n source n in
  crates
  |> Map.set ~key:from ~data:source'
  |> Map.set ~key:to' ~data:(List.append (f cs) dest)
;;

let find_crates f input =
  let { moves; crates } =
    input |> A.parse_string ~consume:Prefix instructionsP |> Result.ok_or_failwith
  in
  moves
  |> List.fold ~init:crates ~f:(move_crate f)
  |> Map.data
  |> List.map ~f:List.hd_exn
  |> String.of_list
;;

let part1 input = find_crates List.rev input
let part2 input = find_crates Fn.id input
