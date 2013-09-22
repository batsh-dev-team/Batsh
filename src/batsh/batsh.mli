type t

module Symbol_table : sig
  type t
  module Scope : sig
    type t
    val add_temporary_variable : t -> Batshast.identifier
    val fold : t -> init: 'a -> f: (string -> bool -> 'a -> 'a) -> 'a
  end
  val scope : t -> string -> Scope.t
  val global_scope : t -> Scope.t
end

val create_from_channel : in_channel -> string -> t
val prettify : out_channel -> t -> unit
val ast : t -> Batshast.asttype
val symtable : t -> Symbol_table.t
