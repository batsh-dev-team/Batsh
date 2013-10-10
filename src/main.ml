open Core.Std

let regular_file = Command.Spec.Arg_type.create (fun filename ->
    match Sys.is_file filename with
    | `Yes -> filename
    | `No | `Unknown ->
      eprintf "%s is not a regular file.\n%!" filename;
      exit 1
  )

let bash =
  Command.basic
    ~summary: "Compile to Bash"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
    ) (fun (filename: string) () ->
        let batsh = Parser.create_from_file filename in
        let bash = Bash.compile batsh in
        printf "%a\n" Bash.print bash
      )

let winbat =
  Command.basic
    ~summary: "Compile to Microsoft Windows Batch"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
    ) (fun (filename: string) () ->
        let batsh = Parser.create_from_file filename in
        let batch = Winbat.compile batsh in
        printf "%a\n" Winbat.print batch
      )

let format =
  Command.basic
    ~summary: "Print formatted and prettified source code"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
    ) (fun (filename: string) () ->
        let batsh = Parser.create_from_file filename in
        Parser.prettify Out_channel.stdout batsh
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
        let batsh = Parser.create_from_file filename in
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
