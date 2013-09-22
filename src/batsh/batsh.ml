open Core.Std

type t = {
  lex: Lexing.lexbuf;
  ast: Batsh_ast.t;
  symtable: Symbol_table.t
}

let parse_and_print_error
    (outx : out_channel)
    (lexbuf : Lexing.lexbuf)
  : Batsh_ast.t =
  let print_position (outx: out_channel) () =
    let pos = lexbuf.Lexing.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.Lexing.pos_fname
      pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
  in
  try
    Parser.program Lexer.read lexbuf
  with
  | Lexer.SyntaxError msg ->
    fprintf outx "%a: %s\n" print_position () msg;
    exit (-1)
  | Parser.Error ->
    fprintf outx "%a: syntax error\n" print_position ();
    exit (-1)

module Symbol_table = struct
  include Symbol_table
end

let create_from_channel (inx: in_channel) (filename: string) : t =
  let lexbuf = Lexing.from_channel inx in
  lexbuf.Lexing.lex_curr_p <- {
    lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename
  };
  let ast = parse_and_print_error stderr lexbuf in
  let symtable = Symbol_table.create ast in
  { lex = lexbuf; ast; symtable }

let create_from_file (filename : string) : t =
  let inx = In_channel.create filename in
  let batsh = create_from_channel inx filename in
  In_channel.close inx;
  batsh

let prettify (outx: out_channel) (batsh: t) =
  fprintf outx "%a\n" Batsh_format.print_ast batsh.ast

let ast (batsh: t) : Batsh_ast.t =
  batsh.ast

let symtable (batsh: t) : Symbol_table.t =
  batsh.symtable
