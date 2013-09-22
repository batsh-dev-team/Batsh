type t

val compile : Batsh.t -> t
val print : out_channel -> t -> unit
