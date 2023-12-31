module P = Util.Parser
open P.Syntax
module StrMap = Map.Make (String)

type attr =
  | X
  | M
  | A
  | S

type state =
  | Reject
  | Accept

type dest =
  | Name of string
  | End of state

type fields =
  { attr : attr
  ; n : int
  ; dest : dest
  }

type rule =
  | LT of fields
  | GT of fields
  | Move of dest

type workflow =
  { name : string
  ; rules : rule list
  }

type part =
  { x : int
  ; m : int
  ; a : int
  ; s : int
  }

type qpart =
  { x' : int * int
  ; m' : int * int
  ; a' : int * int
  ; s' : int * int
  }

type input =
  { workflows : workflow StrMap.t
  ; parts : part list
  }

let attr_p =
  P.choice [ X <$ P.char 'x'; M <$ P.char 'm'; A <$ P.char 'a'; S <$ P.char 's' ]
;;

let name_p =
  P.take_while (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let state_p = P.choice [ Accept <$ P.char 'A'; Reject <$ P.char 'R' ]
let dest_p = P.choice [ (state_p >>| fun s -> End s); (name_p >>| fun n -> Name n) ]

let lt_p =
  let+ attr = attr_p <* P.char '<'
  and+ n = P.integer <* P.char ':'
  and+ dest = dest_p in
  LT { attr; n; dest }
;;

let gt_p =
  let+ attr = attr_p <* P.char '>'
  and+ n = P.integer <* P.char ':'
  and+ dest = dest_p in
  GT { attr; n; dest }
;;

let move_p = dest_p >>| fun d -> Move d
let rule_p = P.choice [ lt_p; gt_p; move_p ]

let workflow_p =
  let+ name = name_p <* P.char '{'
  and+ rules = P.sep_by1 (P.char ',') rule_p <* P.char '}' in
  { name; rules }
;;

let workflows_p =
  let+ workflows = P.sep_by1 P.end_of_line workflow_p in
  workflows |> List.map ~f:(fun w -> w.name, w) |> StrMap.of_alist_exn
;;

let part_p =
  let+ x = P.string "{x=" *> P.integer <* P.char ','
  and+ m = P.string "m=" *> P.integer <* P.char ','
  and+ a = P.string "a=" *> P.integer <* P.char ','
  and+ s = P.string "s=" *> P.integer <* P.char '}' in
  { x; m; a; s }
;;

let parts_p = P.sep_by1 P.end_of_line part_p

let input_p =
  let+ workflows = workflows_p <* P.string "\n\n"
  and+ parts = parts_p in
  { workflows; parts }
;;

let init workflows = Map.find_exn workflows "in"

let solve { workflows; parts } =
  let get_attr { x; m; a; s } = function
    | X -> x
    | M -> m
    | A -> a
    | S -> s
  in
  let exec_rule part = function
    | Move dest -> Some dest
    | LT { attr; n; dest } when get_attr part attr < n -> Some dest
    | GT { attr; n; dest } when get_attr part attr > n -> Some dest
    | _ -> None
  in
  let rec is_accepted workflow part =
    let dest = List.find_map_exn workflow.rules ~f:(exec_rule part) in
    match dest with
    | End Accept -> true
    | End Reject -> false
    | Name n ->
      let workflow' = Map.find_exn workflows n in
      is_accepted workflow' part
  in
  parts
  |> List.filter ~f:(is_accepted (init workflows))
  |> List.sum (module Int) ~f:(fun { x; m; a; s } -> x + m + a + s)
;;

let solve' { workflows } =
  let init_workflow = init workflows in
  let init_qpart = { x' = 1, 4000; m' = 1, 4000; a' = 1, 4000; s' = 1, 4000 } in
  let get_attr { x'; m'; a'; s' } = function
    | X -> x'
    | M -> m'
    | A -> a'
    | S -> s'
  in
  let set_attr qpart attr v =
    match attr with
    | X -> { qpart with x' = v }
    | M -> { qpart with m' = v }
    | A -> { qpart with a' = v }
    | S -> { qpart with s' = v }
  in
  let rec move_dest qpart = function
    | Name dest ->
      let workflow' = Map.find_exn workflows dest in
      results qpart workflow'.rules
    | End Accept -> [ qpart ]
    | End Reject -> []
  and results qpart = function
    | Move dest :: t -> move_dest qpart dest
    | LT { attr; n; dest } :: t ->
      let s, e = get_attr qpart attr in
      if s >= n
      then results qpart t
      else (
        let q1 = set_attr qpart attr (min s (n - 1), min e (n - 1)) in
        let q2 = set_attr qpart attr (max s n, max e n) in
        move_dest q1 dest @ results q2 t)
    | GT { attr; n; dest } :: t ->
      let s, e = get_attr qpart attr in
      if e <= n
      then results qpart t
      else (
        let q1 = set_attr qpart attr (max s (n + 1), max e (n + 1)) in
        let q2 = set_attr qpart attr (min s n, min e n) in
        move_dest q1 dest @ results q2 t)
    | [] -> []
  in
  let vals (s, e) = e - s + 1 in
  let totals { x'; m'; a'; s' } = vals x' * vals m' * vals a' * vals s' in
  results init_qpart init_workflow.rules |> List.sum (module Int) ~f:totals
;;

let part1 input = input |> P.parse_exn input_p |> solve
let part2 input = input |> P.parse_exn input_p |> solve'
