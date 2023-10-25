module A = Angstrom

module Syntax = struct
  module Let_syntax = A.Let_syntax.Let_syntax

  let ( let+ ) = A.( let+ )
  let ( and+ ) = A.( and+ )
  let ( let* ) = A.( let* )

  let ( >>| ), ( *> ), ( <* ), ( <|> ), ( <$> ) =
    A.(( >>| ), ( *> ), ( <* ), ( <|> ), ( <$> ))
  ;;

  let ( $> ) p a = p >>| const a
  let ( <$ ) a p = p >>| const a
end

open Syntax

let integerP =
  let%map tokens =
    A.take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Int.of_string tokens
;;
