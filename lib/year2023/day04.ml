module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)
module P = Util.Parser
open P.Syntax

type card =
  { id : int
  ; winners : IntSet.t
  ; picks : IntSet.t
  }

let nums_p = P.sep_by1 P.spaces P.integer

let card_p =
  let%map id = P.string "Card" *> P.spaces *> P.integer <* P.char ':' <* P.spaces
  and winners = nums_p <* P.spaces <* P.char '|' <* P.spaces
  and picks = nums_p in
  { id; winners = IntSet.of_list winners; picks = IntSet.of_list picks }
;;

let cards_p = P.sep_by1 P.end_of_line card_p
let num_matches card = card.winners |> Set.inter card.picks |> Set.length

let play counts card =
  let count = Map.find_exn counts card.id in
  let new_cards = List.range (card.id + 1) (card.id + 1 + num_matches card) in
  List.fold new_cards ~init:counts ~f:(fun m key ->
    Util.Map.alter m ~key ~f:(fun c -> c + count))
;;

let initial_counts cards =
  cards |> List.map ~f:(fun card -> card.id, 1) |> IntMap.of_alist_exn
;;

let part1 input =
  input
  |> P.parse_exn cards_p
  |> List.map ~f:num_matches
  |> List.filter ~f:(fun l -> l > 0)
  |> List.sum (module Int) ~f:(fun l -> Int.pow 2 (l - 1))
;;

let part2 input =
  let cards = input |> P.parse_exn cards_p in
  cards |> List.fold ~init:(initial_counts cards) ~f:play |> Map.data |> Util.List.sum_int
;;
