include Angstrom

module Syntax = struct
  include Angstrom.Let_syntax

  let ( >>= ), ( >>| ), ( *> ), ( <* ), ( <|> ), ( <?> ), ( <$> ) =
    ( >>= ), ( >>| ), ( *> ), ( <* ), ( <|> ), ( <?> ), ( <$> )
  ;;

  let ( let* ), ( let+ ), ( and+ ) = ( let* ), ( let+ ), ( and+ )
  let ( $> ) p a = p >>| const a
  let ( <$ ) a p = p >>| const a
end

let digits =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let integer = digits >>| Int.of_string

let signed_integer =
  let open Syntax in
  let+ sign = 1 <$ char '+' <|> (-1 <$ char '-') <|> return 1
  and+ n = integer in
  sign * n
;;

let spaces =
  take_while1 (function
    | ' ' -> true
    | _ -> false)
  >>| ignore
;;

let parse_exn parser input =
  input |> parse_string ~consume:Prefix parser |> Result.ok_or_failwith
;;

let parse_all_exn parser input =
  input |> parse_string ~consume:All parser |> Result.ok_or_failwith
;;
