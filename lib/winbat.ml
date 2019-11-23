type t = {
  batsh : Parser.t;
  batch_ast : Winbat_ast.t;
  batch_ast_expanded : Winbat_ast.t;
}

let compile (batsh : Parser.t) : t =
  let batch_ast = Winbat_compile.compile batsh in
  let batch_ast_expanded = Winbat_functions.expand batch_ast in
  {batsh; batch_ast; batch_ast_expanded}

let print (batch : t) : string =
  let buf = Buffer.create 1024 in
  Winbat_format.print buf batch.batch_ast_expanded;
  Buffer.contents buf

let ast ?(expand_functions=true) (winbat : t) : Winbat_ast.t =
  if expand_functions then
    winbat.batch_ast_expanded
  else
    winbat.batch_ast
