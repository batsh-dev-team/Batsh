open Core.Std
open Lexer
open Lexing

let parse_and_print_error (lexbuf: Lexing.lexbuf) =
  let print_position (outx: out_channel) (lexbuf: Lexing.lexbuf) =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in
  try
    Parser.prog Lexer.read lexbuf
  with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let parse_and_prettify (lexbuf: Lexing.lexbuf) =
  let ast = parse_and_print_error lexbuf in
  printf "%a\n" Statement.print_ast ast

let compile_to_bash (lexbuf: Lexing.lexbuf) =
  let ast = parse_and_print_error lexbuf in
  let bash_ast = Bash.compile ast in
  printf "%a\n" Bash.print bash_ast

let main (filename: string) (format: bool) () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  if format then
    parse_and_prettify lexbuf
  else
    compile_to_bash lexbuf;
  In_channel.close inx

let () =
  let regular_file = Command.Spec.Arg_type.create (fun filename ->
    match Sys.is_file filename with
    | `Yes -> filename
    | `No | `Unknown ->
      eprintf "%s is not a regular file.\n%!" filename;
      exit 1
  )
  in
  Command.basic
    ~summary:"Batsh"
    ~readme:(fun () -> "Write once and runs on both UNIX and Windows.")
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
      +> flag "-format" no_arg ~doc:" print prettified source code"
    ) main
  |> Command.run ~version:"0.0" ~build_info:""
