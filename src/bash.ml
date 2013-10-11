type t = {
  batsh : Parser.t;
  bash_ast : Bash_ast.t;
  bash_ast_expanded : Bash_ast.t;
}

let compile (batsh : Parser.t) : t =
  let bash_ast = Bash_compile.compile batsh in
  let bash_ast_expanded = Bash_functions.expand bash_ast in
  {batsh; bash_ast; bash_ast_expanded}

let print (bash : t) : string =
  let buf = Buffer.create 1024 in
  Bash_format.print buf bash.bash_ast_expanded;
  Buffer.contents buf
