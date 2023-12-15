module Key = struct
  type t = char list list [@@deriving sexp, compare, hash]
end

let rotate grid = grid |> List.transpose_exn |> List.map ~f:List.rev
let parse input = input |> String.split_lines |> List.map ~f:String.to_list |> rotate

let shift_rocks l =
  let aux (rocks, l) = function
    | 'O' -> 'O' :: rocks, l
    | '.' -> rocks, '.' :: l
    | '#' -> [], ('#' :: rocks) @ l
    | _ -> rocks, l
  in
  let rocks, l' = List.fold l ~init:([], []) ~f:aux in
  rocks @ l' |> List.rev
;;

let load grid =
  grid
  |> List.mapi ~f:(fun i c -> if Char.equal c 'O' then i + 1 else 0)
  |> Util.List.sum_int
;;

let cycle grid =
  List.fold (List.range 0 4) ~init:grid ~f:(fun acc _ ->
    acc |> List.map ~f:shift_rocks |> rotate)
;;

let run_cycles n grid =
  let cache = Hashtbl.create (module Key) in
  let rec aux n grid =
    match n with
    | 0 -> grid
    | n ->
      (match Hashtbl.find cache grid with
       | Some i -> grid |> cycle |> aux (Int.rem n (i - n) - 1)
       | None ->
         Hashtbl.add_exn cache ~key:grid ~data:n;
         grid |> cycle |> aux (n - 1))
  in
  aux n grid
;;

let part1 input =
  input |> parse |> List.map ~f:shift_rocks |> List.sum (module Int) ~f:load
;;

let part2 input = input |> parse |> run_cycles 1000000000 |> List.sum (module Int) ~f:load
