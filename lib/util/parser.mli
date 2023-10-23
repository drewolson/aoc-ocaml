module A = Angstrom

val integerP : int A.t
val ( $> ) : 'a A.t -> 'b -> 'b A.t
val ( <$ ) : 'b -> 'a A.t -> 'b A.t
