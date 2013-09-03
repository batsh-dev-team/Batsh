open Core.Std
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try
    Parser.prog Lexer.read lexbuf
  with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  let ast = parse_with_error lexbuf in
  printf "%a\n" Statement.print_ast ast

let main (filename: string) (format :bool) () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  if format then
    parse_and_print lexbuf
  else
    failwith "Not implemented yet";
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
