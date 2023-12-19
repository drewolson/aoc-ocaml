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

type rule =
  | LT of
      { attr : attr
      ; n : int
      ; dest : dest
      }
  | GT of
      { attr : attr
      ; n : int
      ; dest : dest
      }
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

type range = int * int

type template =
  { x' : range
  ; m' : range
  ; a' : range
  ; s' : range
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
  let%map attr = attr_p <* P.char '<'
  and n = P.integer <* P.char ':'
  and dest = dest_p in
  LT { attr; n; dest }
;;

let gt_p =
  let%map attr = attr_p <* P.char '>'
  and n = P.integer <* P.char ':'
  and dest = dest_p in
  GT { attr; n; dest }
;;

let move_p = dest_p >>| fun d -> Move d
let rule_p = P.choice [ lt_p; gt_p; move_p ]

let workflow_p =
  let%map name = name_p <* P.char '{'
  and rules = P.sep_by1 (P.char ',') rule_p <* P.char '}' in
  { name; rules }
;;

let workflows_p =
  let%map workflows = P.sep_by1 P.end_of_line workflow_p in
  workflows |> List.map ~f:(fun w -> w.name, w) |> StrMap.of_alist_exn
;;

let part_p =
  let%map x = P.string "{x=" *> P.integer <* P.char ','
  and m = P.string "m=" *> P.integer <* P.char ','
  and a = P.string "a=" *> P.integer <* P.char ','
  and s = P.string "s=" *> P.integer <* P.char '}' in
  { x; m; a; s }
;;

let parts_p = P.sep_by1 P.end_of_line part_p

let input_p =
  let%map workflows = workflows_p <* P.string "\n\n"
  and parts = parts_p in
  { workflows; parts }
;;

let init workflows = Map.find_exn workflows "in"

let solve { workflows; parts } =
  let of_attr { x; m; a; s } = function
    | X -> x
    | M -> m
    | A -> a
    | S -> s
  in
  let exec_rule part = function
    | Move dest -> Some dest
    | LT { attr; n; dest } when of_attr part attr < n -> Some dest
    | GT { attr; n; dest } when of_attr part attr > n -> Some dest
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
  let start = { x' = 1, 4000; m' = 1, 4000; a' = 1, 4000; s' = 1, 4000 } in
  let of_attr { x'; m'; a'; s' } = function
    | X -> x'
    | M -> m'
    | A -> a'
    | S -> s'
  in
  let set_attr template attr v =
    match attr with
    | X -> { template with x' = v }
    | M -> { template with m' = v }
    | A -> { template with a' = v }
    | S -> { template with s' = v }
  in
  let rec move_dest template = function
    | Name dest ->
      let workflow' = Map.find_exn workflows dest in
      results template workflow'.rules
    | End Accept -> [ template ]
    | End Reject -> []
  and results template = function
    | Move dest :: t -> move_dest template dest
    | LT { attr; n; dest } :: t ->
      let s, e = of_attr template attr in
      if s >= n
      then results template t
      else (
        let t1 = set_attr template attr (min s (n - 1), min e (n - 1)) in
        let t2 = set_attr template attr (max s n, max e n) in
        move_dest t1 dest @ results t2 t)
    | GT { attr; n; dest } :: t ->
      let s, e = of_attr template attr in
      if e <= n
      then results template t
      else (
        let t1 = set_attr template attr (max s (n + 1), max e (n + 1)) in
        let t2 = set_attr template attr (min s n, min e n) in
        move_dest t1 dest @ results t2 t)
    | [] -> []
  in
  let vals (s, e) = if s = e then 0 else e - s + 1 in
  let totals { x'; m'; a'; s' } = vals x' * vals m' * vals a' * vals s' in
  results start (init workflows).rules |> List.sum (module Int) ~f:totals
;;

let part1 input = input |> P.parse_exn input_p |> solve
let part2 input = input |> P.parse_exn input_p |> solve'
