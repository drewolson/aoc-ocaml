module Coord = struct
  type t = int * int [@@deriving equal, compare, sexp]
end

module IntMap = Map.Make (Int)
module CoordMap = Map.Make (Coord)

let parse input = input |> String.split_lines |> List.map ~f:String.to_list

let expand amount galaxy =
  let open Z in
  let x_counts =
    galaxy
    |> List.transpose_exn
    |> List.mapi ~f:(fun i line ->
      i, if List.for_all line ~f:(Char.equal '.') then ~$amount else ~$1)
  in
  let y_counts =
    galaxy
    |> List.mapi ~f:(fun i line ->
      i, if List.for_all line ~f:(Char.equal '.') then ~$amount else ~$1)
  in
  IntMap.of_alist_exn x_counts, IntMap.of_alist_exn y_counts
;;

let to_grid galaxy =
  galaxy
  |> List.concat_mapi ~f:(fun y line -> List.mapi line ~f:(fun x c -> (x, y), c))
  |> CoordMap.of_alist_exn
;;

let pairs grid =
  let rec all_pairs = function
    | a :: rest -> List.map rest ~f:(fun b -> a, b) @ all_pairs rest
    | [] -> []
  in
  grid |> Map.filter ~f:(Char.equal '#') |> Map.keys |> all_pairs
;;

let range a b = if a < b then List.range a b else List.range b a

let distance x_counts y_counts ((x1, y1), (x2, y2)) =
  let open Z in
  let x_dist =
    range x1 x2 |> List.fold ~init:~$0 ~f:(fun sum x -> Map.find_exn x_counts x + sum)
  in
  let y_dist =
    range y1 y2 |> List.fold ~init:~$0 ~f:(fun sum y -> Map.find_exn y_counts y + sum)
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
  |> List.fold ~init:Z.zero ~f:Z.add
  |> Z.to_string
;;

let part1 input = solve 2 input
let part2 input = solve 1000000 input
