open Core.Std
open Winbat_ast

let rec expand_command (name : varstring) (exprs : varstrings) =
  match name with
  | `Str "println" ->
    `Call (`Str "echo", exprs)
  | `Str "print" ->
    `Call (`Str "echo.", exprs)
  | `Str "call" -> (
      match exprs with
      | cmd :: args ->
        expand_command cmd args
      | [] ->
        failwith "call must have at least 1 argument."
    )
  | _ ->
    `Call (name, exprs)

let rec expand_statement (stmt : statement) : statement =
  match stmt with
  | `Call (name, exprs) ->
    expand_command name exprs
  (*   | If (expr, stmt) ->
       If (expand_expression expr, expand_statement stmt)
       | IfElse (expr, then_stmt, else_stmt) ->
       IfElse (expand_expression expr,
              expand_statement then_stmt,
              expand_statement else_stmt)
       | While (expr, stmt) ->
       While (expand_expression expr, expand_statement stmt)
       | Block stmts ->
       Block (expand_statements stmts) *)
  | `Assignment _
  | `ArithAssign _
  | `Comment _ | `Raw _ | `Label _ | `Goto _ | `Empty -> stmt

and expand_statements (stmts: statements) : statements =
  List.map stmts ~f: expand_statement

let expand (ast : t) : t =
  expand_statements ast
