type t = {
  batsh : Batsh.t;
  batch_ast : Batch_ast.t
}

let compile (batsh : Batsh.t) : t =
  let batch_ast = Batch_compile.compile batsh in
  {batsh; batch_ast}

let print (outx : out_channel) (batch : t) : unit =
  Batch_format.print outx batch.batch_ast
