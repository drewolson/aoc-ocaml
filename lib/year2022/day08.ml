let make_grid input =
  input
  |> String.split_lines
  |> List.map ~f:(fun l ->
    l
    |> String.to_list
    |> List.map ~f:(fun c -> c |> Char.to_string |> Int.of_string)
    |> Array.of_list)
  |> Array.of_list
;;

let grid_size grid =
  let max_x = Array.length grid.(0) in
  let max_y = Array.length grid in
  max_x, max_y
;;

let coords grid =
  let open List.Let_syntax in
  let max_x, max_y = grid_size grid in
  let%bind x = List.init max_x ~f:Fn.id
  and y = List.init max_y ~f:Fn.id in
  return (x, y)
;;

let rays x y grid =
  let max_x, max_y = grid_size grid in
  [ List.init x ~f:(fun x' -> x', y) |> List.rev
  ; List.init (max_x - x - 1) ~f:(fun x' -> x' + x + 1, y)
  ; List.init y ~f:(fun y' -> x, y') |> List.rev
  ; List.init (max_y - y - 1) ~f:(fun y' -> x, y' + y + 1)
  ]
;;

let visible grid (x, y) =
  let h = grid.(x).(y) in
  grid
  |> rays x y
  |> List.exists ~f:(fun ray ->
    ray |> List.for_all ~f:(fun (x', y') -> grid.(x').(y') < h))
;;

let scenic_score grid (x, y) =
  let h = grid.(x).(y) in
  grid
  |> rays x y
  |> List.map ~f:(fun ray ->
    let vis, rest = List.split_while ray ~f:(fun (x', y') -> h > grid.(x').(y')) in
    List.length vis + min (List.length rest) 1)
  |> List.fold ~init:1 ~f:( * )
;;

let part1 input =
  let grid = make_grid input in
  coords grid |> List.filter ~f:(visible grid) |> List.length
;;

let part2 input =
  let grid = make_grid input in
  coords grid |> List.map ~f:(scenic_score grid) |> Util.List.max_int
;;
