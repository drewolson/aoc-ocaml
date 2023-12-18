module Pq = Pairing_heap

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
  let queue = Pq.create ~cmp:(fun (a, _, _) (b, _, _) -> compare a b) () in
  let expand_steps dir init_loss coords =
    coords
    |> List.filter_map ~f:(fun coord ->
      let%map.Option loss = Map.find grid coord in
      loss, coord)
    |> List.folding_map ~init:init_loss ~f:(fun sum (loss, coord) ->
      let loss' = sum + loss in
      loss', (loss', coord, dir))
  in
  let next_steps loss (x, y) = function
    | N | S ->
      let left = range |> List.map ~f:(fun i -> x - i, y) |> expand_steps W loss in
      let right = range |> List.map ~f:(fun i -> x + i, y) |> expand_steps E loss in
      List.drop left drop @ List.drop right drop
    | E | W ->
      let up = range |> List.map ~f:(fun i -> x, y - i) |> expand_steps N loss in
      let down = range |> List.map ~f:(fun i -> x, y + i) |> expand_steps S loss in
      List.drop up drop @ List.drop down drop
  in
  let rec loop visited =
    match Pq.pop_exn queue with
    | loss, coord, _ when Coord.equal coord goal -> loss
    | _, coord, dir when Set.mem visited (coord, dir) -> loop visited
    | loss, coord, dir ->
      next_steps loss coord dir
      |> List.filter ~f:(fun (_, coord, dir) -> not @@ Set.mem visited (coord, dir))
      |> List.iter ~f:(Pq.add queue);
      loop (Set.add visited (coord, dir))
  in
  next_steps 0 (0, 0) N @ next_steps 0 (0, 0) E |> List.iter ~f:(Pq.add queue);
  loop NodeSet.empty
;;

let part1 input = input |> parse |> solve 3 0
let part2 input = input |> parse |> solve 10 3
