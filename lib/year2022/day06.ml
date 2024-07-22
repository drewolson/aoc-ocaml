module CharSet = Set.Make (Char)

let is_unique l = List.stable_dedup ~compare:Char.compare l |> List.equal Char.equal l

let find_char i =
  let rec find_char' n l =
    let next = List.take l i in
    if is_unique next then n + i else find_char' (n + 1) (List.tl_exn l)
  in
  find_char' 0
;;

let part1 input = input |> String.to_list |> find_char 4
let part2 input = input |> String.to_list |> find_char 14
