let rotate grid = grid |> Util.List.transpose |> List.map ~f:List.rev
let parse input = input |> String.lines |> List.map ~f:String.to_list |> rotate

let tilt grid =
  let aux (rocks, l) = function
    | 'O' -> 'O' :: rocks, l
    | '.' -> rocks, '.' :: l
    | '#' -> [], ('#' :: rocks) @ l
    | _ -> rocks, l
  in
  let shift_rocks l =
    let rocks, l' = List.fold_left l ~init:([], []) ~f:aux in
    rocks @ l' |> List.rev
  in
  List.map grid ~f:shift_rocks
;;

let load grid =
  grid
  |> List.mapi ~f:(fun i c -> if Char.equal c 'O' then i + 1 else 0)
  |> Util.List.sum_int
;;

let run_cycles n =
  let cache = Hashtbl.create 1000 in
  let cycle grid =
    List.fold_left List.(0 -- 3) ~init:grid ~f:(fun acc _ -> acc |> tilt |> rotate)
  in
  let rec aux n grid =
    match n with
    | 0 -> grid
    | n ->
      (match Hashtbl.find_opt cache grid with
       | Some i -> grid |> cycle |> aux (Int.rem n (i - n) - 1)
       | None ->
         Hashtbl.add cache grid n;
         grid |> cycle |> aux (n - 1))
  in
  aux n
;;

let part1 input =
  input |> parse |> tilt |> List.fold_left ~init:0 ~f:(fun acc g -> acc + load g)
;;

let part2 input =
  input
  |> parse
  |> run_cycles 1000000000
  |> List.fold_left ~init:0 ~f:(fun acc g -> acc + load g)
;;
