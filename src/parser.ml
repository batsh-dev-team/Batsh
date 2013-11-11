open Core_kernel.Std

type t = {
  lex: Lexing.lexbuf;
  ast: Batsh_ast.t;
  symtable: Symbol_table.t;
}

exception ParseError of string
exception SemanticError of string

let parse (lexbuf : Lexing.lexbuf) : Batsh_ast.t =
  let print_position () () =
    let pos = lexbuf.Lexing.lex_curr_p in
    sprintf "%s:%d:%d"
      pos.Lexing.pos_fname
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
  in
  try
    Parser_yacc.program Lexer.read lexbuf
  with
  | Lexer.SyntaxError msg ->
    let err = sprintf "%a: %s" print_position () msg in
    raise (ParseError err)
  | Parsing.Parse_error ->
    let err = sprintf "%a: syntax error" print_position () in
    raise (ParseError err)

let create_from_lexbuf (lexbuf : Lexing.lexbuf) (filename: string) : t =
  lexbuf.Lexing.lex_curr_p <- {
    lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename
  };
  let ast = parse lexbuf in
  let () =
    try
      Semantic_checker.check ast
    with
    | Semantic_checker.Error msg ->
      raise (SemanticError (sprintf "Semantic error: %s" msg))
  in
  let symtable = Symbol_table.create ast in
  { lex = lexbuf; ast; symtable; }

let create_from_channel (inx: in_channel) (filename: string) : t =
  let lexbuf = Lexing.from_channel inx in
  create_from_lexbuf lexbuf filename

let create_from_file (filename : string) : t =
  let inx = In_channel.create filename in
  let batsh = create_from_channel inx filename in
  In_channel.close inx;
  batsh

let create_from_string (source : string) : t =
  let lexbuf = Lexing.from_string source in
  create_from_lexbuf lexbuf "input"

let prettify (batsh : t) : string =
  let buf = Buffer.create 1024 in
  Batsh_format.print_ast buf batsh.ast;
  Buffer.contents buf

let ast (batsh: t) : Batsh_ast.t =
  batsh.ast

let symtable (batsh: t) : Symbol_table.t =
  batsh.symtable
