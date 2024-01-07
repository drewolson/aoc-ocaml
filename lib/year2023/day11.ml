module Coord = struct
  type t = int * int [@@deriving eq, ord]
end

module IntMap = Map.Make (Int)
module CoordMap = Map.Make (Coord)

let parse input = input |> String.lines |> List.map ~f:String.to_list

let expand amount galaxy =
  let build_counts l =
    l
    |> List.mapi ~f:(fun i line ->
      i, if List.for_all line ~f:(Char.equal '.') then amount else 1)
    |> IntMap.of_list
  in
  build_counts (Util.List.transpose galaxy), build_counts galaxy
;;

let to_grid galaxy =
  galaxy
  |> List.mapi ~f:(fun y line -> List.mapi line ~f:(fun x c -> (x, y), c))
  |> List.concat
  |> CoordMap.of_list
;;

let pairs grid =
  let rec all_pairs = function
    | a :: rest -> List.map rest ~f:(fun b -> a, b) @ all_pairs rest
    | [] -> []
  in
  grid
  |> CoordMap.filter (fun _ v -> Char.equal v '#')
  |> CoordMap.keys
  |> List.of_iter
  |> all_pairs
;;

let range a b = if a < b then List.(a --^ b) else List.(b --^ a)

let distance x_counts y_counts ((x1, y1), (x2, y2)) =
  let x_dist =
    range x1 x2 |> List.fold_left ~init:0 ~f:(fun acc i -> acc + IntMap.find i x_counts)
  in
  let y_dist =
    range y1 y2 |> List.fold_left ~init:0 ~f:(fun acc i -> acc + IntMap.find i y_counts)
  in
  x_dist + y_dist
;;

let solve amount input =
  let galaxy = parse input in
  let x_counts, y_counts = expand amount galaxy in
  galaxy
  |> to_grid
  |> pairs
  |> List.map ~f:(distance x_counts y_counts)
  |> Util.List.sum_int
;;

let part1 input = solve 2 input
let part2 input = solve 1000000 input
