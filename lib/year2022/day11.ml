module IntMap = Map.Make (Int)
module P = Util.Parser
open P.Syntax

type monkey =
  { id : int
  ; items : Z.t list
  ; op : Z.t -> Z.t
  ; div_test : Z.t
  ; if_true : int
  ; if_false : int
  }

let items_p = P.sep_by1 (P.string ", ") P.integer

let op_p =
  P.choice
    [ P.string "new = old + " *> P.integer >>| Z.of_int >>| Z.( + )
    ; P.string "new = old * " *> P.integer >>| Z.of_int >>| Z.( * )
    ; (P.string "new = old * old" $> fun z -> Z.(z ** 2))
    ]
;;

let div_test_p = P.string "divisible by " *> P.integer >>| Z.of_int
let throw_p = P.string "throw to monkey " *> P.integer

let monkey_p =
  let+ id = P.string "Monkey " *> P.integer <* P.string ":" <* P.end_of_line
  and+ items = P.string "  Starting items: " *> items_p <* P.end_of_line
  and+ op = P.string "  Operation: " *> op_p <* P.end_of_line
  and+ div_test = P.string "  Test: " *> div_test_p <* P.end_of_line
  and+ if_true = P.string "    If true: " *> throw_p <* P.end_of_line
  and+ if_false = P.string "    If false: " *> throw_p <* P.end_of_line in
  { id; items = List.map items ~f:Z.of_int; op; div_test; if_true; if_false }
;;

let monkeys_p = P.sep_by1 P.end_of_line monkey_p

let parse_input input =
  input |> P.parse_exn monkeys_p |> List.map ~f:(fun m -> m.id, m) |> IntMap.of_list
;;

let empty_counts monkeys =
  List.(0 --^ IntMap.cardinal monkeys)
  |> List.fold_left ~init:IntMap.empty ~f:(fun m i -> IntMap.add i Z.zero m)
;;

let rec play_rounds n reduce counts monkeys =
  let update_monkey (cs, ms) i =
    let m = IntMap.find i ms in
    let cs' =
      IntMap.update
        i
        (fun v -> Some Z.(Option.value v ~default:~$0 + ~$(List.length m.items)))
        cs
    in
    let items' = List.map ~f:(Fun.compose m.op reduce) m.items in
    let true_items, false_items =
      List.partition items' ~f:(fun n -> Z.(equal (n mod m.div_test) ~$0))
    in
    let ms' =
      ms
      |> IntMap.add i { m with items = [] }
      |> IntMap.update m.if_true (function
        | None -> None
        | Some mt -> Some { mt with items = mt.items @ true_items })
      |> IntMap.update m.if_false (function
        | None -> None
        | Some mf -> Some { mf with items = mf.items @ false_items })
    in
    cs', ms'
  in
  if n = 0
  then counts
  else
    List.(0 --^ IntMap.cardinal monkeys)
    |> List.fold_left ~init:(counts, monkeys) ~f:update_monkey
    |> fun (c, m) -> play_rounds (n - 1) reduce c m
;;

let solve monkeys reduce rounds =
  play_rounds rounds reduce IntMap.empty monkeys
  |> IntMap.values
  |> List.of_iter
  |> List.sort ~cmp:(fun a b -> -Z.compare a b)
  |> List.take 2
  |> List.fold_left ~init:Z.one ~f:Z.( * )
  |> Z.to_string
;;

let part1 input =
  let monkeys = parse_input input in
  let reduce n = Z.(n / ~$3) in
  solve monkeys reduce 20
;;

let part2 input =
  let monkeys = parse_input input in
  let prod =
    monkeys
    |> IntMap.values
    |> List.of_iter
    |> List.map ~f:(fun m -> m.div_test)
    |> List.fold_left ~init:Z.one ~f:Z.( * )
  in
  let reduce n = Z.(n mod prod) in
  solve monkeys reduce 10000
;;
