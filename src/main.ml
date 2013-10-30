open Core_kernel.Std

module Command = Core.Command
module Sys = Core.Std.Sys

let regular_file = Command.Spec.Arg_type.create (fun filename ->
    match Sys.is_file filename with
    | `Yes -> filename
    | `No | `Unknown ->
      eprintf "%s is not a regular file.\n%!" filename;
      exit 1
  )

let parse_with_error (filename : string) : Parser.t =
  try
    Parser.create_from_file filename
  with
  | Parser.ParseError msg ->
    eprintf "%s\n" msg;
    exit 1

let bash =
  Command.basic
    ~summary: "Compile to Bash"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
    ) (fun (filename: string) () ->
        let batsh = parse_with_error filename in
        let bash = Bash.compile batsh in
        let code = Bash.print bash in
        printf "%s\n" code
      )

let winbat =
  Command.basic
    ~summary: "Compile to Microsoft Windows Batch"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
      +> flag "-ast" no_arg ~doc:" Print abstract syntax tree"
    ) (fun (filename : string) (print_ast : bool) () ->
        let batsh = parse_with_error filename in
        let winbat = Winbat.compile batsh in
        if not print_ast then
          let code = Winbat.print winbat in
          printf "%s\n" code
        else
          let ast = Winbat.ast winbat in
          printf "%a\n" Sexp.output_hum (Winbat_ast.sexp_of_t ast)
      )

let format =
  Command.basic
    ~summary: "Print formatted and prettified source code"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
    ) (fun (filename: string) () ->
        let batsh = parse_with_error filename in
        let code = Parser.prettify batsh in
        printf "%s\n" code
      )

let symbols =
  Command.basic
    ~summary: "Print symbol table"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
    ) (fun (filename : string) () ->
        let batsh = parse_with_error filename in
        let symtable_sexp = Symbol_table.sexp_of_t (Parser.symtable batsh) in
        printf "%a\n" Sexp.output_hum symtable_sexp
      )

let () =
  Command.group
    ~summary: "Batsh"
    ~readme: (fun () -> "Write once and runs with Bash and Batsh")
    [
      ("bash", bash);
      ("bat", winbat);
      ("symbols", symbols);
      ("format", format)
    ]
  |> Command.run ~version: "0.0" ~build_info: ""
