open Core.Std
open Batshast

type t = {
  lex: Lexing.lexbuf;
  ast: Batshast.asttype
}

let parse_and_print_error (lexbuf: Lexing.lexbuf) : Batshast.asttype =
  let print_position (outx: out_channel) (lexbuf: Lexing.lexbuf) =
    let pos = lexbuf.Lexing.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.Lexing.pos_fname
      pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
  in
  try
    Parser.program Lexer.read lexbuf
  with
  | Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let create_from_channel (inx: in_channel) (filename: string) : t =
  let lexbuf = Lexing.from_channel inx in
  lexbuf.Lexing.lex_curr_p <- {
    lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename
  };
  let ast = parse_and_print_error lexbuf in
  { lex = lexbuf; ast}

let prettify (batsh: t) =
  printf "%a\n" Batshformat.print_ast batsh.ast

let ast (batsh: t) : Batshast.asttype =
  batsh.ast
