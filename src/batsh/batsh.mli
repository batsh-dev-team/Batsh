type t

val create_from_channel : in_channel -> string -> t

val prettify : t -> unit

val ast : t -> Batshast.asttype
