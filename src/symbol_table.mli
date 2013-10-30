open Core_kernel.Std

type t

module Scope : sig
  type t
  val is_function : t -> bool
  val is_global_variable : t -> name: string -> bool
  val add_temporary_variable : t -> Batsh_ast.identifier
  val fold : t -> init: 'a -> f: (string -> bool -> 'a -> 'a) -> 'a
end

val create : Batsh_ast.t -> t
val scope : t -> string -> Scope.t
val global_scope : t -> Scope.t
val sexp_of_t : t -> Sexp.t
val is_function : t -> string -> bool
