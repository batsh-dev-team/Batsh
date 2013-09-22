type t = Bash_ast.asttype

let compile (batsh : Batsh.t) : t =
  Bash_compile.compile batsh

let print (outx : out_channel) (bash : t) : unit =
  Bash_format.print outx bash
