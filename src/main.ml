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
        let batsh = Batsh.create_from_file filename in
        let bash = Bash.compile batsh in
        printf "%a\n" Bash.print bash
      )

let batch =
  Command.basic
    ~summary: "Compile to Microsoft Windows Batch"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
    ) (fun (filename: string) () ->
        let batsh = Batsh.create_from_file filename in
        let batch = Batch.compile batsh in
        printf "%a\n" Batch.print batch
      )

let format =
  Command.basic
    ~summary: "Print formatted and prettified source code"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
    ) (fun (filename: string) () ->
        let batsh = Batsh.create_from_file filename in
        Batsh.prettify Out_channel.stdout batsh
      )

let ast =
  Command.basic
    ~summary: "Print syntax tree of source code"
    Command.Spec.(
      empty
      +> anon ("filename" %: regular_file)
      +> flag "-symbols" no_arg ~doc:" symbol table instead"
    ) (fun (filename : string) (symbols : bool) () ->
        let batsh = Batsh.create_from_file filename in
        if not symbols then
          let ast_sexp = Batsh_ast.sexp_of_t (Batsh.ast batsh) in
          printf "%a\n" Sexp.output_hum ast_sexp
        else
          let symtable_sexp = Batsh.Symbol_table.sexp_of_t (Batsh.symtable batsh) in
          printf "%a\n" Sexp.output_hum symtable_sexp
      )

let () =
  Command.group
    ~summary: "Batsh"
    ~readme: (fun () -> "Write once and runs with Bash and Batsh")
    [
      ("bash", bash);
      ("bat", batch);
      ("ast", ast);
      ("format", format)
    ]
  |> Command.run ~version: "0.0" ~build_info: ""
