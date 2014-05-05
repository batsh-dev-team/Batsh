open Core_kernel.Std
open Cmdliner

(* Options common to all commands *)

type output_type = Code | Ast | Symbols

type copts = {
  output_type : output_type;
  output_file : string option
}

let copts_sect = "COMMON OPTIONS"

let copts_t =
  let docs = copts_sect in

  let output_type =
    let doc = "Print abstract syntax tree instead." in
    let quiet = Ast, Arg.info ["ast"] ~docs ~doc in

    let doc = "Print symbol table instead." in
    let verbose = Symbols, Arg.info ["symbols"] ~docs ~doc in
    Arg.(last & vflag_all [Code] [quiet; verbose])
  in

  let output_file =
    let doc = "Write output to $(docv)." in
    let opts = ["o"; "output"] in
    Arg.(value & opt (some string) None & info opts ~docs ~doc ~docv:"FILE")
  in
  let copts_cons output_type output_file = { output_type; output_file } in
  Term.(pure copts_cons $ output_type $ output_file)

let get_outx opts =
  match opts.output_file with
  | Some filename -> Out_channel.create filename
  | None -> Out_channel.stdout

let print_common opts ~batsh ~code ~ast =
  let outx = get_outx opts in
  match opts.output_type with
  | Code ->
    fprintf outx "%s\n" (Lazy.force code)
  | Ast ->
    fprintf outx "%a\n" Sexp.output_hum (Lazy.force ast)
  | Symbols ->
    let symtable_sexp = Symbol_table.sexp_of_t (Parser.symtable batsh) in
    fprintf outx "%a\n" Sexp.output_hum symtable_sexp

(* Commands *)

let parse_with_error (filename : string) : Parser.t =
  try
    Parser.create_from_file filename
  with
  | Parser.ParseError msg ->
    eprintf "%s\n" msg;
    exit 1
  | Parser.SemanticError msg ->
    eprintf "%s\n" msg;
    exit 1

let bash =
  let doc = "Compile $(docv) to Bash script." in
  let t =
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:"FILE")
  in
  let cmd opts (filename : string) =
    let batsh = parse_with_error filename in
    let bash =
      try
        Bash.compile batsh
      with
      | Errors.SemanticError (msg, context) ->
        eprintf "%s\n%s\n" msg context;
        exit 1
    in
    let code = lazy (Bash.print bash) in
    let ast = lazy (Bash.ast bash |> Bash_ast.sexp_of_t) in
    print_common opts ~code ~ast ~batsh
  in
  Term.(pure cmd $ copts_t $ t),
  Term.info "bash" ~doc:"Compile to Bash script."

let winbat =
  let doc = "Compile $(docv) to Windows Batch script." in
  let t =
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:"FILE")
  in
  let cmd opts (filename : string) =
    let batsh = parse_with_error filename in
    let winbat =
      try
        Winbat.compile batsh
      with
      | Errors.SemanticError (msg, context) ->
        eprintf "%s\n%s\n" msg context;
        exit 1
    in
    let code = lazy (Winbat.print winbat) in
    let ast = lazy (Winbat.ast winbat |> Winbat_ast.sexp_of_t) in
    print_common opts ~code ~ast ~batsh
  in
  Term.(pure cmd $ copts_t $ t),
  Term.info "winbat" ~doc:"Compile to Windows Batch script."

let batsh =
  let doc = "Format $(docv)." in
  let t =
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:"FILE")
  in
  let cmd opts (filename : string) =
    let batsh = parse_with_error filename in
    let code = lazy (Parser.prettify batsh) in
    let ast = lazy (Parser.ast batsh |> Batsh_ast.sexp_of_t) in
    print_common opts ~code ~ast ~batsh
  in
  Term.(pure cmd $ copts_t $ t),
  Term.info "batsh" ~doc:"Format source file."

let default_cmd =
  let doc = Version.description in
  Term.(ret (pure (fun _ -> `Help (`Plain, None)) $ (Term.pure ()) )),
  Term.info "batsh" ~version:Version.version ~doc

let () =
  match Term.eval_choice default_cmd [bash; winbat; batsh] with
  | `Error _ -> exit 1
  | _ -> ()
