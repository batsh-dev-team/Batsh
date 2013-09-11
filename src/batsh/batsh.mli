type t
type asttype = Batshast.asttype

module Parser : sig
  val parse_and_print_error : t -> asttype
end

module Format : sig
  val prettify : t -> unit
end

val create_from_channel : in_channel -> string -> t

val compile_to_bash : asttype -> Bash.asttype
