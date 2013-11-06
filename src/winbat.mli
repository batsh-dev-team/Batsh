type t

val compile : Parser.t -> t
val print : t -> string
val ast : ?expand_functions:bool -> t -> Winbat_ast.t 
