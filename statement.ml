type identifier = string

type expression =
  | Bool of bool
  | Float of float
  | Int of int
  | List of expression list
  | String of string
  | Identifier of identifier
  | Plus of (expression * expression)
  | Minus of (expression * expression)
  | Multiply of (expression * expression)
  | Divide of (expression * expression)
  | Modulo of (expression * expression)
  | Parentheses of expression

type statement = 
  | Block of (statements)
  | Expression of (expression)
  | Assignment of (identifier * expression)
  | If of (expression * statement)
  | IfElse of (expression * statement * statement)
  | Empty

and statements = statement list

open Core.Std

let rec print_expression out (expr: expression) =
  match expr with
  | Identifier identifier -> output_string out identifier
  | Int number -> output_string out (string_of_int number)
  | Bool true  -> output_string out "true"
  | Bool false -> output_string out "false"
  | Plus _ | Minus _ | Multiply _ | Divide _ ->
    print_binary_expression out expr
  | Parentheses expr ->
    output_string out "(";
    print_expression out expr;
    output_string out ")"
  | _ -> output_string out "???"

and print_binary_expression out (expr: expression) : unit =
  match expr with
  | Plus (left, right) ->
    print_expression out left;
    output_string out "+";
    print_expression out right
  | Minus (left, right) ->
    print_expression out left;
    output_string out "-";
    print_expression out right
  | Multiply (left, right) ->
    print_expression out left;
    output_string out "*";
    print_expression out right
  | Divide (left, right) ->
    print_expression out left;
    output_string out "/";
    print_expression out right
  | Modulo (left, right) ->
    print_expression out left;
    output_string out "%";
    print_expression out right
  | _ -> assert false

let rec print_statement out (stmt: statement) =
  match stmt with
  | Block inner_stmts -> print_block_statement out inner_stmts
  | Expression expr ->
    print_expression out expr;
    output_string out ";"
  | Assignment (ident, expr) ->
    output_string out ident;
    output_string out "=";
    print_expression out expr;
    output_string out ";"
  | If (expr, stmt) -> print_if_statement out expr stmt
  | IfElse (expr, thenStmt, elseStmt) ->
    print_if_else_statement out expr thenStmt elseStmt
  | Empty -> ()

and print_statements out (stmts: statements) =
  List.iter stmts ~f: (print_statement out)

and print_block_statement out (inner_stmts: statements) =
  output_string out "{";
  print_statements out inner_stmts;
  output_string out "}"

and print_if_statement out (expr: expression) (stmt: statement) =
  output_string out "if (";
  print_expression out expr;
  output_string out ") ";
  print_statement out stmt

and print_if_else_statement out (expr: expression) (thenStmt: statement) (elseStmt: statement) =
  output_string out "if (";
  print_expression out expr;
  output_string out ") ";
  print_statement out thenStmt;
  output_string out "else";
  print_statement out elseStmt

let print_ast out (stmts: statements) =
  print_statements out stmts

