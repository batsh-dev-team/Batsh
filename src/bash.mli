type t

val compile : Parser.t -> t
val print : out_channel -> t -> unit
