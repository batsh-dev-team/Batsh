open Core.Std
open Bash_ast

let rec expand_leftvalue (lvalue : leftvalue) : leftvalue =
  match lvalue with
  | Identifier _ ->
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
    Command (expand_command name exprs)
  | List (exprs) ->
    List (expand_expressions exprs)
  | StrTemp _ ->
    failwith "StrTemp should be all replaced."
  | String _ | Result _ -> expr

and expand_expressions (exprs : expressions) : expressions =
  List.map exprs ~f: expand_expression

and expand_command (name : expression) (exprs : expressions) =
  let exprs = expand_expressions exprs in
  match name with
  | String "println" ->
    String "echo", (String "-e") :: exprs
  | String "print" ->
    String "echo", (String "-ne") :: exprs
  | String "call" -> (
      match exprs with
      | cmd :: args ->
        expand_command cmd args
      | [] ->
        failwith "call must have at least 1 argument."
    )
  | _ ->
    name, exprs

let rec expand_statement (stmt : statement) : statement =
  match stmt with
  | Assignment (lvalue, expr) ->
    Assignment (expand_leftvalue lvalue, expand_expression expr)
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
  | Comment _ | Local _ | Empty -> stmt

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
