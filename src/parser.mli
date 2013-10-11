open Core.Std

type t
exception ParseError of string

module Symbol_table : sig
  type t
  module Scope : sig
    type t
    val is_function : t -> bool
    val is_global_variable : t -> name: string -> bool
    val add_temporary_variable : t -> Batsh_ast.identifier
    val fold : t -> init: 'a -> f: (string -> bool -> 'a -> 'a) -> 'a
  end
  val scope : t -> string -> Scope.t
  val global_scope : t -> Scope.t
  val sexp_of_t : t -> Sexp.t
  val is_function : t -> string -> bool
end

val create_from_file : string -> t
val create_from_channel : in_channel -> string -> t
val create_from_string : string -> t
val prettify : t -> string
val ast : t -> Batsh_ast.t
val symtable : t -> Symbol_table.t
val split_ast : t -> split_string : bool -> split_list_literal : bool
  -> split_call : bool -> split_string_compare : bool -> split_arithmetic : bool
  -> Batsh_ast.t
