type axis =
  | V of int
  | H of int
[@@deriving equal]

let parse input =
  input
  |> Pcre.split ~pat:"\n\n"
  |> List.map ~f:(fun grid ->
    grid
    |> String.split_lines
    |> List.map ~f:(fun l -> l |> String.to_list |> Array.of_list))
  |> List.map ~f:Array.of_list
;;

let reflection_score g =
  let rec zip l r =
    match l, r with
    | [], _ -> []
    | _, [] -> []
    | a :: ar, b :: br -> (a, b) :: zip ar br
  in
  let is_mirror i grid =
    Array.for_all grid ~f:(fun line ->
      let l, r = List.split_n (Array.to_list line) i in
      zip (List.rev l) r |> List.for_all ~f:(fun (a, b) -> Char.equal a b))
  in
  let aux grid =
    List.range 1 (Array.length grid.(0)) |> List.filter ~f:(fun i -> is_mirror i grid)
  in
  (aux g |> List.map ~f:(fun i -> V i, i))
  @ (aux (Array.transpose_exn g) |> List.map ~f:(fun i -> H i, i * 100))
;;

let reflection_score' g =
  let old_axis = reflection_score g |> List.hd_exn |> fst in
  let swap = function
    | '.' -> '#'
    | _ -> '.'
  in
  let with_swap (x, y) ~f =
    let v = g.(x).(y) in
    g.(x).(y) <- swap v;
    let result = f () in
    g.(x).(y) <- v;
    result
  in
  let coords =
    let open Sequence.Let_syntax in
    let%bind x = Sequence.range 0 (Array.length g) in
    let%map y = Sequence.range 0 (Array.length g.(x)) in
    x, y
  in
  coords
  |> Sequence.find_map ~f:(fun coord ->
    with_swap coord ~f:(fun _ ->
      g
      |> reflection_score
      |> List.find_map ~f:(fun (axis, score) ->
        if equal_axis axis old_axis then None else Some score)))
  |> Option.value_exn
;;

let part1 input =
  input
  |> parse
  |> List.sum (module Int) ~f:(fun g -> reflection_score g |> List.hd_exn |> snd)
;;

let part2 input = input |> parse |> List.sum (module Int) ~f:reflection_score'
