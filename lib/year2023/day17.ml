type dir =
  | N
  | S
  | E
  | W
[@@deriving sexp, compare, equal]

module Coord = struct
  type t = int * int [@@deriving sexp, compare, equal]
end

module Node = struct
  type t = Coord.t * dir [@@deriving sexp, compare, equal]
end

module CoordMap = Map.Make (Coord)
module NodeSet = Set.Make (Node)

let parse input =
  input
  |> String.split_lines
  |> List.concat_mapi ~f:(fun y l ->
    l |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), Char.to_int c - 48))
  |> CoordMap.of_alist_exn
;;

let solve max drop grid =
  let range = List.range ~stop:`inclusive 1 max in
  let goal =
    grid |> Map.keys |> List.max_elt ~compare:Coord.compare |> Option.value_exn
  in
  let cmp_entry (a, _, _) (b, _, _) = compare a b in
  let queue = Pairing_heap.create ~cmp:cmp_entry () in
  let expand_steps d l cs =
    cs
    |> List.filter_map ~f:(fun c ->
      let%map.Option s = Map.find grid c in
      s, c, d)
    |> List.fold ~init:([], l) ~f:(fun (cs, s) (l, c, d) -> (s + l, c, d) :: cs, s + l)
    |> fst
    |> List.rev
  in
  let next_steps l (x, y) = function
    | N | S ->
      let left = range |> List.map ~f:(fun i -> x - i, y) |> expand_steps W l in
      let right = range |> List.map ~f:(fun i -> x + i, y) |> expand_steps E l in
      List.drop left drop @ List.drop right drop
    | E | W ->
      let up = range |> List.map ~f:(fun i -> x, y - i) |> expand_steps N l in
      let down = range |> List.map ~f:(fun i -> x, y + i) |> expand_steps S l in
      List.drop up drop @ List.drop down drop
  in
  let rec loop visited =
    match Pairing_heap.pop_exn queue with
    | l, c, _ when Coord.equal c goal -> l
    | _, c, d when Set.mem visited (c, d) -> loop visited
    | l, c, d ->
      next_steps l c d
      |> List.filter ~f:(fun (_, c, d) -> not @@ Set.mem visited (c, d))
      |> List.iter ~f:(Pairing_heap.add queue);
      loop (Set.add visited (c, d))
  in
  next_steps 0 (0, 0) N @ next_steps 0 (0, 0) E |> List.iter ~f:(Pairing_heap.add queue);
  loop NodeSet.empty
;;

let part1 input = input |> parse |> solve 3 0
let part2 input = input |> parse |> solve 10 3
