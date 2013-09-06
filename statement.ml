open Core.Std

(*********** Definition ***********)

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
  | Concat of (expression * expression)
  | Parentheses of expression
  | Call of (identifier * expression list)

type statement = 
  | Block of (statements)
  | Expression of (expression)
  | Assignment of (identifier * expression)
  | If of (expression * statement)
  | IfElse of (expression * statement * statement)
  | Empty

and statements = statement list

(*********** Output ***********)

let rec print_expression out (expr: expression) =
  match expr with
  | Identifier identifier -> output_string out identifier
  | Int number -> output_string out (string_of_int number)
  | Float number -> output_string out (Float.to_string number)
  | String str -> output_string out (sprintf "\"%s\"" str)
  | Bool true  -> output_string out "true"
  | Bool false -> output_string out "false"
  | Plus _ | Minus _ | Multiply _ | Divide _  | Modulo _ | Concat _ ->
    print_binary_expression out expr
  | Parentheses expr ->
    output_string out "(";
    print_expression out expr;
    output_string out ")"
  | Call (ident, exprs) ->
    output_string out ident;
    output_string out "(";
    print_expressions out exprs;
    output_string out ")"
  | _ -> output_string out "???"

and print_expressions out (exprs: expression list) =
  let num_exprs = List.length exprs in
  List.iteri exprs ~f: (fun i expr ->
    print_expression out expr;
    if i <> num_exprs - 1 then
      output_string out ", "
  )

and print_binary_expression out (expr: expression) =
  match expr with
  | Plus (left, right) ->
    print_expression out left;
    output_string out " + ";
    print_expression out right
  | Minus (left, right) ->
    print_expression out left;
    output_string out " - ";
    print_expression out right
  | Multiply (left, right) ->
    print_expression out left;
    output_string out " * ";
    print_expression out right
  | Divide (left, right) ->
    print_expression out left;
    output_string out " / ";
    print_expression out right
  | Modulo (left, right) ->
    print_expression out left;
    output_string out " % ";
    print_expression out right
  | Concat (left, right) ->
    print_expression out left;
    output_string out " ++ ";
    print_expression out right
  | _ -> assert false

let print_indent out (indent: int) =
  output_string out (String.make indent ' ')

let rec print_statement out (stmt: statement) ~(indent: int) =
  match stmt with
  | Block inner_stmts -> print_block_statement ~indent out inner_stmts
  | Expression expr ->
      print_expression out expr;
      output_string out ";"
  | Assignment (ident, expr) ->
      output_string out ident;
      output_string out " = ";
      print_expression out expr;
      output_string out ";"
  | If (expr, stmt) ->
      print_if_statement out expr stmt ~indent
  | IfElse (expr, thenStmt, elseStmt) ->
      print_if_else_statement out expr thenStmt elseStmt ~indent
  | Empty -> ()

and print_statements out (stmts: statements) ~(indent: int) =
  List.iter stmts ~f: (fun stmt ->
    print_indent out indent;
    print_statement out stmt ~indent;
    output_string out "\n"
  )

and print_block_statement out (inner_stmts: statements) ~(indent: int) =
  output_string out "{\n";
  print_statements out inner_stmts ~indent:(indent + 2);
  output_string out "\n";
  print_indent out indent;
  output_string out "}"

and print_if_statement
    out (expr: expression) (stmt: statement) ~(indent: int) =
  output_string out "if (";
  print_expression out expr;
  output_string out ") ";
  print_statement out stmt ~indent

and print_if_else_statement
    out
    (expr: expression)
    (thenStmt: statement)
    (elseStmt: statement)
    ~(indent: int) =
  output_string out "if (";
  print_expression out expr;
  output_string out ") ";
  print_statement out thenStmt ~indent;
  output_string out " else ";
  print_statement out elseStmt ~indent

let print_ast out (stmts: statements) =
  print_statements out stmts ~indent: 0
