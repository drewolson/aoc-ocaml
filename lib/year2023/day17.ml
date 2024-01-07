type dir =
  | N
  | S
  | E
  | W
[@@deriving eq, ord]

module Coord = struct
  type t = int * int [@@deriving eq, ord]
end

module Node = struct
  type t = Coord.t * dir [@@deriving eq, ord]
end

module Key = struct
  type t = int * Coord.t * dir

  let leq (a, _, _) (b, _, _) = a <= b
end

module CoordMap = Map.Make (Coord)
module NodeSet = Set.Make (Node)
module KeyHeap = Heap.Make (Key)

let parse input =
  input
  |> String.lines
  |> List.mapi ~f:(fun y l ->
    l |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), Char.to_int c - 48))
  |> List.concat
  |> CoordMap.of_list
;;

let solve max drop grid =
  let range = List.(1 -- max) in
  let goal =
    grid
    |> CoordMap.keys
    |> List.of_iter
    |> List.reduce_exn ~f:(fun a b -> if Coord.compare a b <= 0 then b else a)
  in
  let expand_steps dir init_loss coords =
    coords
    |> List.filter_map ~f:(fun coord ->
      grid |> CoordMap.find_opt coord |> Option.map (fun loss -> loss, coord))
    |> List.fold_map ~init:init_loss ~f:(fun sum (loss, coord) ->
      let loss' = sum + loss in
      loss', (loss', coord, dir))
    |> snd
  in
  let next_steps loss (x, y) = function
    | N | S ->
      let left = range |> List.map ~f:(fun i -> x - i, y) |> expand_steps W loss in
      let right = range |> List.map ~f:(fun i -> x + i, y) |> expand_steps E loss in
      List.drop drop left @ List.drop drop right
    | E | W ->
      let up = range |> List.map ~f:(fun i -> x, y - i) |> expand_steps N loss in
      let down = range |> List.map ~f:(fun i -> x, y + i) |> expand_steps S loss in
      List.drop drop up @ List.drop drop down
  in
  let rec loop visited queue =
    let queue', key = KeyHeap.take_exn queue in
    match key with
    | loss, coord, _ when Coord.equal coord goal -> loss
    | _, coord, dir when NodeSet.mem (coord, dir) visited -> loop visited queue'
    | loss, coord, dir ->
      next_steps loss coord dir
      |> List.filter ~f:(fun (_, coord, dir) -> not @@ NodeSet.mem (coord, dir) visited)
      |> List.fold_left ~init:queue' ~f:KeyHeap.add
      |> loop (NodeSet.add (coord, dir) visited)
  in
  next_steps 0 (0, 0) N @ next_steps 0 (0, 0) E
  |> List.fold_left ~init:KeyHeap.empty ~f:KeyHeap.add
  |> loop NodeSet.empty
;;

let part1 input = input |> parse |> solve 3 0
let part2 input = input |> parse |> solve 10 3
