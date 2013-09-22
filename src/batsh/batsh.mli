type t

module Symbol_table : sig
  type t
  module Scope : sig
    type t
    val add_temporary_variable : t -> Batsh_ast.identifier
    val fold : t -> init: 'a -> f: (string -> bool -> 'a -> 'a) -> 'a
  end
  val scope : t -> string -> Scope.t
  val global_scope : t -> Scope.t
end

val create_from_file : string -> t
val create_from_channel : in_channel -> string -> t
val prettify : out_channel -> t -> unit
val ast : t -> Batsh_ast.t
val symtable : t -> Symbol_table.t
