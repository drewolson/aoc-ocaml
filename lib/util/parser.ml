include Angstrom

module Syntax = struct
  module Let_syntax = Angstrom.Let_syntax.Let_syntax

  let ( let+ ) = ( let+ )
  let ( and+ ) = ( and+ )
  let ( let* ) = ( let* )

  let ( >>| ), ( *> ), ( <* ), ( <|> ), ( <$> ) =
    ( >>| ), ( *> ), ( <* ), ( <|> ), ( <$> )
  ;;

  let ( $> ) p a = p >>| const a
  let ( <$ ) a p = p >>| const a
end

let integer =
  let open Syntax in
  let%map tokens =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Int.of_string tokens
;;

let parse_exn parser input =
  input |> parse_string ~consume:Prefix parser |> Result.ok_or_failwith
;;

let parse_all_exn parser input =
  input |> parse_string ~consume:All parser |> Result.ok_or_failwith
;;
