module CharSet = Set.Make (Char)
module CharMap = Map.Make (Char)

let mapping =
  let little = List.init 26 ~f:(fun i -> Char.of_int_exn (i + 97), i + 1) in
  let big = List.init 26 ~f:(fun i -> Char.of_int_exn (i + 65), i + 27) in
  little |> List.append big |> CharMap.of_list
;;

let find_match line =
  let chars = String.to_list line in
  let a, b = List.take_drop (String.length line / 2) chars in
  let inter = CharSet.inter (CharSet.of_list a) (CharSet.of_list b) in
  inter |> CharSet.to_list |> List.hd
;;

let part1 input =
  input
  |> String.lines
  |> List.map ~f:find_match
  |> List.map ~f:(fun c -> CharMap.find c mapping)
  |> List.fold_left ~init:0 ~f:( + )
;;

let part2 input =
  input
  |> String.lines
  |> List.map ~f:(fun line -> line |> String.to_list |> CharSet.of_list)
  |> List.chunks 3
  |> List.map ~f:(fun sets ->
    sets |> List.reduce_exn ~f:CharSet.inter |> CharSet.to_list |> List.hd)
  |> List.map ~f:(fun c -> CharMap.find c mapping)
  |> List.fold_left ~init:0 ~f:( + )
;;
