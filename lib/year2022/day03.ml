module CharSet = Set.Make (Char)
module CharMap = Map.Make (Char)

let mapping =
  let little = List.init 26 ~f:(fun i -> Char.of_int_exn (i + 97), i + 1) in
  let big = List.init 26 ~f:(fun i -> Char.of_int_exn (i + 65), i + 27) in
  little |> List.append big |> CharMap.of_alist_exn
;;

let find_match line =
  let chars = String.to_list line in
  let a, b = List.split_n chars (String.length line / 2) in
  let inter = Set.inter (CharSet.of_list a) (CharSet.of_list b) in
  inter |> Set.to_list |> List.hd_exn
;;

let part1 input =
  input
  |> String.split_lines
  |> List.map ~f:find_match
  |> List.sum (module Int) ~f:(Map.find_exn mapping)
;;

let part2 input =
  input
  |> String.split_lines
  |> List.map ~f:(fun line -> line |> String.to_list |> CharSet.of_list)
  |> List.chunks_of ~length:3
  |> List.map ~f:(fun sets ->
    sets |> List.reduce_exn ~f:Set.inter |> Set.to_list |> List.hd_exn)
  |> List.sum (module Int) ~f:(Map.find_exn mapping)
;;
