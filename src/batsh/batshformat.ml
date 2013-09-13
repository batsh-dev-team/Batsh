open Core.Std
open Batshast

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
  | String str -> output_string out (sprintf "\"%s\"" str)
  | Bool true  -> output_string out "true"
  | Bool false -> output_string out "false"
  | Plus _ | Minus _ | Multiply _ | Divide _  | Modulo _ | Concat _
  | Equal _ | NotEqual _ | Greater _ | Less _ | GreaterEqual _ | LessEqual _ ->
    print_binary_expression out expr
  | Parentheses expr ->
    fprintf out "(%a)" print_expression expr
  | Call (ident, exprs) ->
    fprintf out "%s(%a)" ident print_expressions exprs
  | List exprs ->
    fprintf out "[%a]" print_expressions exprs

and print_expressions out (exprs: expression list) =
  let num_exprs = List.length exprs in
  List.iteri exprs ~f: (fun i expr ->
      print_expression out expr;
      if i <> num_exprs - 1 then
        output_string out ", "
    )

and print_binary_expression out (expr: expression) =
  let print_binary operator left right =
    fprintf out "%a %s %a"
      print_expression left operator print_expression right
  in
  match expr with
  | Plus (left, right) ->
    print_binary "+" left right
  | Minus (left, right) ->
    print_binary "-" left right
  | Multiply (left, right) ->
    print_binary "*" left right
  | Divide (left, right) ->
    print_binary "/" left right
  | Modulo (left, right) ->
    print_binary "%" left right
  | Concat (left, right) ->
    print_binary "++" left right
  | Equal (left, right) ->
    print_binary "==" left right
  | NotEqual (left, right) ->
    print_binary "!=" left right
  | Greater (left, right) ->
    print_binary ">" left right
  | Less (left, right) ->
    print_binary "<" left right
  | GreaterEqual (left, right) ->
    print_binary ">=" left right
  | LessEqual (left, right) ->
    print_binary "<=" left right
  | _ -> assert false

let rec print_statement out (stmt: statement) ~(indent: int) =
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
  | Empty -> ()

and print_statements = Formatutil.print_statements ~f: print_statement

and print_block_statement out (inner_stmts: statements) ~(indent: int) =
  let print_statements_indented = print_statements ~indent:(indent + 2) in
  fprintf out "{\n%a%a}"
    print_statements_indented inner_stmts Formatutil.print_indent indent

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

let print_ast out (ast: asttype) =
  print_statements out ast ~indent: 0
