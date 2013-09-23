type t = {
  batsh : Batsh.t;
  bash_ast : Bash_ast.t;
  bash_ast_expanded : Bash_ast.t;
}

let compile (batsh : Batsh.t) : t =
  let bash_ast = Bash_compile.compile batsh in
  let bash_ast_expanded = Bash_functions.expand bash_ast in
  {batsh; bash_ast; bash_ast_expanded}

let print (outx : out_channel) (bash : t) : unit =
  Bash_format.print outx bash.bash_ast_expanded
