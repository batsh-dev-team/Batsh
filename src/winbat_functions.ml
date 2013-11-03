open Core_kernel.Std
open Winbat_ast

let rec expand_command (name : varstrings) (args : parameters) =
  match name with
  | [`Str "bash"] ->
    `Empty
  | [`Str "batch"] -> (
      match args with
      | [[`Str raw]] ->
        `Raw raw
      | _ ->
        failwith "batch raw command must have 1 argument of string literal."
    )
  | [`Str "println"] -> (
      match args with
      | [] ->
        `Call ([`Str "echo:"], [])
      | _ ->
        `Call ([`Str "echo"], args)
    )
  | [`Str "print"] ->
    `Call ([`Str "echo | set /p ="], [] :: args)
  | [`Str "call"] -> (
      match args with
      | cmd :: real_args ->
        expand_command cmd real_args
      | [] ->
        failwith "call must have at least 1 argument."
    )
  | [`Str "readdir"] ->
    `Call ([`Str "dir /w"], args)
  | _ ->
    `Call (name, args)

let rec expand_statement (stmt : statement) : statement =
  match stmt with
  | `Call (name, exprs) ->
    expand_command name exprs
  | `Output (lvalue, name, exprs) ->
    let expaned = expand_command name exprs in (
      match expaned with
      | `Call (name, exprs) -> `Output (lvalue, name, exprs)
      | _ -> failwith (sprintf "command do not have a return value.")
    )
  | `If (condition, stmts) ->
    `If (condition, expand_statements stmts)
  | `IfElse (condition, then_stmts, else_stmts) ->
    `IfElse (condition,
             expand_statements then_stmts,
             expand_statements else_stmts)
  | `Assignment _
  | `ArithAssign _
  | `Comment _ | `Raw _ | `Label _ | `Goto _ | `Empty -> stmt

and expand_statements (stmts: statements) : statements =
  List.map stmts ~f: expand_statement

let expand (ast : t) : t =
  expand_statements ast
