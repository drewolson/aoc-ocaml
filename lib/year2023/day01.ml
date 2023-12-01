module StrMap = Map.Make (String)

let words = [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
let digits = List.range 1 10 |> List.map ~f:Int.to_string
let mapping = List.zip_exn words digits |> StrMap.of_alist_exn
let re_f = words |> List.append digits |> String.concat ~sep:"|" |> Pcre.regexp

let re_b =
  words
  |> List.map ~f:String.rev
  |> List.append digits
  |> String.concat ~sep:"|"
  |> Pcre.regexp
;;

let numberify m = Map.find mapping m |> Option.value ~default:m

let to_digit' line =
  let a = line |> Util.Regex.first ~rex:re_f |> numberify in
  let b = line |> String.rev |> Util.Regex.first ~rex:re_b |> String.rev |> numberify in
  Int.of_string (a ^ b)
;;

let to_digit line =
  let first_num s =
    s |> String.find ~f:Char.is_digit |> Option.value_exn |> String.of_char
  in
  Int.of_string (first_num line ^ first_num (String.rev line))
;;

let part1 input = input |> String.split_lines |> List.map ~f:to_digit |> Util.List.sum_int

let part2 input =
  input |> String.split_lines |> List.map ~f:to_digit' |> Util.List.sum_int
;;
