open Core.Std
open Batshast

type t = Lexing.lexbuf
type asttype = Batshast.asttype

module Parser = struct
  let parse_and_print_error (lexbuf: t) : asttype =
    let print_position (outx: out_channel) (lexbuf: t) =
      let pos = lexbuf.Lexing.lex_curr_p in
      fprintf outx "%s:%d:%d" pos.Lexing.pos_fname
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
    in
    try
      Parser.prog Lexer.read lexbuf
    with
    | Lexer.SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      exit (-1)
    | Parser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)
end

module Format = struct
  include Batshformat
  let prettify (batsh: t) =
    let ast = Parser.parse_and_print_error batsh in
    printf "%a\n" print_ast ast
end

let create_from_channel (inx: in_channel) (filename: string) : t =
  let lexbuf = Lexing.from_channel inx in
  lexbuf.Lexing.lex_curr_p <- {
    lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename
  };
  lexbuf

let compile_to_bash (ast: asttype) : Bash.asttype =
  Bash.Compile.compile ast
