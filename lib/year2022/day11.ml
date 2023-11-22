module P = Util.Parser
open P.Syntax
module IntMap = Map.Make (Int)

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
  let%map id = P.string "Monkey " *> P.integer <* P.string ":" <* P.end_of_line
  and items = P.string "  Starting items: " *> items_p <* P.end_of_line
  and op = P.string "  Operation: " *> op_p <* P.end_of_line
  and div_test = P.string "  Test: " *> div_test_p <* P.end_of_line
  and if_true = P.string "    If true: " *> throw_p <* P.end_of_line
  and if_false = P.string "    If false: " *> throw_p <* P.end_of_line in
  { id; items = List.map items ~f:Z.of_int; op; div_test; if_true; if_false }
;;

let monkeys_p = P.sep_by1 P.end_of_line monkey_p

let parse_input input =
  input |> P.parse_exn monkeys_p |> List.map ~f:(fun m -> m.id, m) |> IntMap.of_alist_exn
;;

let empty_counts monkeys =
  monkeys
  |> Map.length
  |> List.range 0
  |> List.fold ~init:IntMap.empty ~f:(fun m i -> Map.add_exn m ~key:i ~data:Z.zero)
;;

let rec play_rounds n reduce counts monkeys =
  let update_monkey (cs, ms) i =
    let m = Map.find_exn ms i in
    let cs' =
      Map.update cs i ~f:(fun v ->
        Z.(Option.value ~default:~$0 v + ~$(List.length m.items)))
    in
    let items' = List.map ~f:(Fn.compose reduce m.op) m.items in
    let true_items, false_items =
      List.partition_tf items' ~f:(fun n -> Z.(equal (n mod m.div_test) ~$0))
    in
    let ms' =
      ms
      |> Map.set ~key:i ~data:{ m with items = [] }
      |> Util.Map.alter ~key:m.if_true ~f:(fun mt ->
        { mt with items = mt.items @ true_items })
      |> Util.Map.alter ~key:m.if_false ~f:(fun mf ->
        { mf with items = mf.items @ false_items })
    in
    cs', ms'
  in
  if n = 0
  then counts
  else
    List.range 0 (Map.length monkeys)
    |> List.fold ~init:(counts, monkeys) ~f:update_monkey
    |> fun (c, m) -> play_rounds (n - 1) reduce c m
;;

let solve monkeys reduce rounds =
  play_rounds rounds reduce IntMap.empty monkeys
  |> Map.data
  |> List.sort ~compare:(fun a b -> -Z.compare a b)
  |> Util.List.take ~n:2
  |> List.fold ~init:Z.one ~f:Z.( * )
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
    |> Map.data
    |> List.map ~f:(fun m -> m.div_test)
    |> List.fold ~init:Z.one ~f:Z.( * )
  in
  let reduce n = Z.(n mod prod) in
  solve monkeys reduce 10000
;;
