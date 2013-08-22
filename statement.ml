type expression = [
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of expression list
  | `String of string
]

type statement = [
  | `Block of (statements)
  | `Expression of (expression)
  | `If of (expression * statement)
  | `Empty
]
and statements = statement list

open Core.Std

let print_expression out (expr: expression) =
  match expr with
  | `Bool true  -> output_string out "true"
  | `Bool false -> output_string out "false"
  | _ -> output_string out "???"

let rec print_statement out (stmt: statement) =
  match stmt with
  | `Block inner_stmts -> print_block_statement out inner_stmts
  | `Expression expr ->
    print_expression out expr;
    output_string out ";"
  | `If (expr, stmt) -> print_if_statement out expr stmt
  | `Empty -> ()

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

let print_ast out (stmts: statements) =
  print_statements out stmts

