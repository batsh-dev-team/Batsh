type t
exception ParseError of string

val create_from_file : string -> t
val create_from_channel : in_channel -> string -> t
val create_from_string : string -> t
val prettify : t -> string
val ast : t -> Batsh_ast.t
val symtable : t -> Symbol_table.t
val split_ast : t -> split_string : bool -> split_list_literal : bool
  -> split_call : bool -> split_string_compare : bool -> split_arithmetic : bool
  -> Batsh_ast.t
