type t

val compile : Parser.t -> t
val print : t -> string
val ast : ?expand_functions:bool -> t -> Bash_ast.t 
