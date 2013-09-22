open Core.Std
open Batsh_ast

let rec print_lvalue out (lvalue: leftvalue) =
  match lvalue with
  | Identifier ident ->
    output_string out ident
  | ListAccess (lvalue, expr) ->
    fprintf out "%a[%a]" print_lvalue lvalue print_expression expr

and print_expression out (expr: expression) =
  match expr with
  | Leftvalue lvalue -> print_lvalue out lvalue
  | Int number -> output_string out (string_of_int number)
  | Float number -> output_string out (Float.to_string number)
  | String str -> fprintf out "\"%s\"" (Formatutil.escape str)
  | Bool true  -> output_string out "true"
  | Bool false -> output_string out "false"
  | ArithUnary (operator, expr) ->
    fprintf out "%s%a" operator print_expression expr
  | ArithBinary binary | StrBinary binary ->
    print_binary_expression out binary
  | Parentheses expr ->
    fprintf out "(%a)" print_expression expr
  | Call (ident, exprs) ->
    fprintf out "%s(%a)" ident print_expressions exprs
  | List exprs ->
    fprintf out "[%a]" print_expressions exprs

and print_expressions (outx: out_channel) (exprs: expression list) =
  Formatutil.print_separate_list outx exprs
    ~f: print_expression ~separator: ", "

and print_binary_expression
    (outx :out_channel)
    (operator, left, right)
  =
  fprintf outx "%a %s %a"
    print_expression left operator print_expression right

let rec print_statement out (stmt: statement) ~(indent: int) =
  let () = match stmt with
    | Block _ -> ()
    | _ ->
      Formatutil.print_indent out indent in
  match stmt with
  | Comment comment ->
    fprintf out "//%s" comment
  | Block inner_stmts -> print_block_statement ~indent out inner_stmts
  | Expression expr ->
    print_expression out expr;
    output_string out ";"
  | Assignment (lvalue, expr) ->
    fprintf out "%a = %a;" print_lvalue lvalue print_expression expr
  | If (expr, stmt) ->
    print_if_statement out expr stmt ~indent
  | IfElse (expr, thenStmt, elseStmt) ->
    print_if_else_statement out expr thenStmt elseStmt ~indent
  | While (expr, stmt) ->
    print_while_statement out expr stmt ~indent
  | Global ident ->
    fprintf out "global %s;" ident
  | Empty -> ()

and print_statements = Formatutil.print_statements ~f: print_statement

and print_block_statement out (inner_stmts: statements) ~(indent: int) =
  let print_statements_indented = print_statements ~indent:(indent + 2) in
  fprintf out "{\n%a\n%a}"
    print_statements_indented inner_stmts
    Formatutil.print_indent indent

and print_if_while_statement
    out (name: string) (expr: expression) (stmt: statement) ~(indent: int) =
  fprintf out "%s (%a) " name print_expression expr;
  print_statement out stmt ~indent

and print_if_statement
    out (expr: expression) (stmt: statement) ~(indent: int) =
  print_if_while_statement out "if" expr stmt ~indent

and print_if_else_statement
    (out: out_channel)
    (expr: expression)
    (thenStmt: statement)
    (elseStmt: statement)
    ~(indent: int) =
  print_if_statement out expr thenStmt ~indent;
  output_string out " else ";
  print_statement out elseStmt ~indent

and print_while_statement
    out (expr: expression) (stmt: statement) ~(indent: int) =
  print_if_while_statement out "while" expr stmt ~indent

let print_params (outx: out_channel) (params: identifiers) =
  Formatutil.print_separate_list outx params ~f: output_string ~separator: ", "

let print_function (outx: out_channel) (name, params, stmts) =
  fprintf outx "function %s (%a) {\n%a\n}"
    name
    print_params params
    (print_statements ~indent: 2) stmts

let print_toplevel (outx: out_channel) (topl: toplevel) ~indent =
  match topl with
  | Statement stmt ->
    print_statement outx stmt ~indent
  | Function func ->
    print_function outx func

let print_ast (outx: out_channel) (ast: t) =
  Formatutil.print_statements outx ast ~f: print_toplevel ~indent: 0
