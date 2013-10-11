open Core.Std

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
    ) (fun (filename: string) () ->
        let batsh = parse_with_error filename in
        let batch = Winbat.compile batsh in
        let code = Winbat.print batch in
        printf "%s\n" code
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

let ast =
  Command.basic
    ~summary: "Print syntax tree of source code"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
      +> flag "-symbols" no_arg ~doc:" show symbol table instead"
      +> flag "-split-string" no_arg
          ~doc:" split string expressions into assignments"
      +> flag "-split-list" no_arg
          ~doc:" split list literals into assignments"
      +> flag "-split-call" no_arg
          ~doc:" split call expressions into assignments"
      +> flag "-split-string-compare" no_arg
          ~doc:" split string comparison expressions into assignments"
      +> flag "-split-arithmetic" no_arg
          ~doc:" split arithmetic expressions into assignments"
    ) (fun (filename : string)
        (symbols : bool)
        (split_string : bool)
        (split_list_literal : bool)
        (split_call : bool)
        (split_string_compare : bool)
        (split_arithmetic : bool)
        () ->
        let batsh = parse_with_error filename in
        if not symbols then
          let ast =
            if split_string || split_list_literal || split_call then
              Parser.split_ast batsh
                ~split_string
                ~split_list_literal
                ~split_call
                ~split_string_compare
                ~split_arithmetic
            else
              Parser.ast batsh
          in
          let ast_sexp = Batsh_ast.sexp_of_t ast in
          printf "%a\n" Sexp.output_hum ast_sexp
        else
          let symtable_sexp = Parser.Symbol_table.sexp_of_t (Parser.symtable batsh) in
          printf "%a\n" Sexp.output_hum symtable_sexp
      )

let () =
  Command.group
    ~summary: "Batsh"
    ~readme: (fun () -> "Write once and runs with Bash and Batsh")
    [
      ("bash", bash);
      ("bat", winbat);
      ("ast", ast);
      ("format", format)
    ]
  |> Command.run ~version: "0.0" ~build_info: ""
