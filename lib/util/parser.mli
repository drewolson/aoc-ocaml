module A = Angstrom

module Syntax : sig
  module Let_syntax : sig
    include module type of A.Let_syntax.Let_syntax
  end

  val ( *> ) : 'a A.t -> 'b A.t -> 'b A.t
  val ( <* ) : 'a A.t -> 'b A.t -> 'a A.t
  val ( >>| ) : 'a A.t -> ('a -> 'b) -> 'b A.t
  val ( <$> ) : ('a -> 'b) -> 'a A.t -> 'b A.t
  val ( <|> ) : 'a A.t -> 'a A.t -> 'a A.t
  val ( $> ) : 'a A.t -> 'b -> 'b A.t
  val ( <$ ) : 'b -> 'a A.t -> 'b A.t
end

val integerP : int A.t
