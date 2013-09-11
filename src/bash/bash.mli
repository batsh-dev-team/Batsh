open Bashast

type asttype

module Compile : sig
  val compile : Batshast.asttype -> asttype
end

module Format : sig
  val print : out_channel -> asttype -> unit
end
