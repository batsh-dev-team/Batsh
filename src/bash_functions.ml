open Core_kernel.Std
open Bash_ast

let rec expand_leftvalue (lvalue : leftvalue) : leftvalue =
  match lvalue with
  | Identifier _ | EntireList _ | Cardinal _ ->
    lvalue
  | ListAccess (lvalue, arith) ->
    ListAccess (expand_leftvalue lvalue, arith)

let rec expand_expression (expr : expression) : expression =
  match expr with
  | Variable lvalue ->
    Variable (expand_leftvalue lvalue)
  | StrBinary (operator, left, right) ->
    StrBinary (operator, expand_expression left, expand_expression right)
  | Command (name, exprs) ->
    expand_command name exprs
  | List (exprs) ->
    List (expand_expressions exprs)
  | String _ | Result _ | Raw _ | TestUnary _ -> expr

and expand_expressions (exprs : expressions) : expressions =
  List.map exprs ~f: expand_expression

and expand_command (name : expression) (exprs : expressions) =
  let exprs = expand_expressions exprs in
  match name with
  | String "bash" -> (
      match exprs with
      | [String raw] ->
        Raw raw
      | _ ->
        failwith "bash raw command must have 1 argument of string literal."
    )
  | String "batch" ->
    failwith "batch raw command can not be a part of expression."
  | String "println" ->
    Command (String "echo", (String "-e") :: exprs)
  | String "print" ->
    Command (String "echo", (String "-ne") :: exprs)
  | String "call" -> (
      match exprs with
      | cmd :: args ->
        expand_command cmd args
      | [] ->
        failwith "call must have at least 1 argument."
    )
  | String "len" -> (
      match exprs with
      | [Variable lvalue] | [Result (Leftvalue lvalue)] ->
        Variable (Cardinal (EntireList lvalue))
      | _ ->
        failwith "len must have exactly 1 argument."
    )
  | String "readdir" ->
    Command (String "ls", exprs)
  | _ ->
    Command (name, exprs)

let rec expand_statement (stmt : statement) : statement =
  match stmt with
  | Assignment (lvalue, expr) ->
    Assignment (expand_leftvalue lvalue, expand_expression expr)
  | Expression (Command (String "batch", _)) ->
    Empty
  | Expression expr ->
    Expression (expand_expression expr)
  | If (expr, stmt) ->
    If (expand_expression expr, expand_statement stmt)
  | IfElse (expr, then_stmt, else_stmt) ->
    IfElse (expand_expression expr,
            expand_statement then_stmt,
            expand_statement else_stmt)
  | While (expr, stmt) ->
    While (expand_expression expr, expand_statement stmt)
  | Block stmts ->
    Block (expand_statements stmts)
  | Comment _ | Local _ | Empty | Return -> stmt

and expand_statements (stmts: statements) : statements =
  List.map stmts ~f: expand_statement

let expand_function ((name : identifier), (stmts : statements))
  : (identifier * statements) =
  (name, expand_statements stmts)

let expand_toplevel (topl : toplevel) : toplevel =
  match topl with
  | Statement stmt ->
    Statement (expand_statement stmt)
  | Function func ->
    Function (expand_function func)

let expand (ast : t) : t =
  List.map ast ~f: expand_toplevel
