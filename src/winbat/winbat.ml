type t = {
  batsh : Batsh.t;
  batch_ast : Winbat_ast.t
}

let compile (batsh : Batsh.t) : t =
  let batch_ast = Winbat_compile.compile batsh in
  {batsh; batch_ast}

let print (outx : out_channel) (batch : t) : unit =
  Winbat_format.print outx batch.batch_ast
