type t
type asttype
type symtable

module Parser : sig
  val parse_and_print_error : t -> asttype
end

module Format : sig
  val prettify : t -> unit
end

module Symbol_table : sig
  val create : asttype -> symtable
  val find_function : symtable -> string -> bool
end

val create_from_channel : in_channel -> string -> t

val compile_to_bash : asttype -> Bash.asttype
