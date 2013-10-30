open Core_kernel.Std
open Batsh_ast

let rec print_lvalue (buf : Buffer.t) (lvalue: leftvalue) =
  match lvalue with
  | Identifier ident ->
    Buffer.add_string buf ident
  | ListAccess (lvalue, expr) ->
    bprintf buf "%a[%a]" print_lvalue lvalue print_expression expr

and print_expression (buf : Buffer.t) (expr: expression) =
  match expr with
  | Leftvalue lvalue -> print_lvalue buf lvalue
  | Int number -> Buffer.add_string buf (string_of_int number)
  | Float number -> Buffer.add_string buf (Float.to_string number)
  | String str -> bprintf buf "\"%s\"" (Formatutil.escape str)
  | Bool true  -> Buffer.add_string buf "true"
  | Bool false -> Buffer.add_string buf "false"
  | ArithUnary (operator, expr) ->
    bprintf buf "%s(%a)" operator print_expression expr
  | ArithBinary binary | StrCompare binary ->
    print_binary_expression buf binary
  | Concat (left, right) ->
    print_binary_expression buf ("++", left, right)
  | Call (ident, exprs) ->
    bprintf buf "%s(%a)" ident print_expressions exprs
  | List exprs ->
    bprintf buf "[%a]" print_expressions exprs

and print_expressions (buf : Buffer.t) (exprs: expression list) =
  Formatutil.print_separate_list buf exprs
    ~f: print_expression ~separator: ", "

and print_binary_expression
    (buf : Buffer.t)
    (operator, left, right)
  =
  bprintf buf "(%a %s %a)"
    print_expression left operator print_expression right

let rec print_statement (buf : Buffer.t) (stmt: statement) ~(indent: int) =
  let () = match stmt with
    | Block _ -> ()
    | _ ->
      Formatutil.print_indent buf indent in
  match stmt with
  | Comment comment ->
    bprintf buf "//%s" comment
  | Block inner_stmts -> print_block_statement ~indent buf inner_stmts
  | Expression expr ->
    print_expression buf expr;
    Buffer.add_string buf ";"
  | Assignment (lvalue, expr) ->
    bprintf buf "%a = %a;" print_lvalue lvalue print_expression expr
  | If (expr, stmt) ->
    print_if_statement buf expr stmt ~indent
  | IfElse (expr, thenStmt, elseStmt) ->
    print_if_else_statement buf expr thenStmt elseStmt ~indent
  | While (expr, stmt) ->
    print_while_statement buf expr stmt ~indent
  | Global ident ->
    bprintf buf "global %s;" ident
  | Return (Some expr) ->
    bprintf buf "return %a;" print_expression expr
  | Return None ->
    bprintf buf "return;"
  | Empty -> ()

and print_statements = Formatutil.print_statements ~f: print_statement

and print_block_statement
    (buf : Buffer.t)
    (inner_stmts : statements)
    ~(indent : int)
  =
  let print_statements_indented = print_statements ~indent:(indent + 2) in
  bprintf buf "{\n%a\n%a}"
    print_statements_indented inner_stmts
    Formatutil.print_indent indent

and print_if_while_statement
    (buf : Buffer.t)
    (name : string)
    (expr : expression)
    (stmt : statement) 
    ~(indent : int)
  =
  bprintf buf "%s (%a) " name print_expression expr;
  print_statement buf stmt ~indent

and print_if_statement
    (buf : Buffer.t)
    (expr: expression)
    (stmt: statement)
    ~(indent: int)
  =
  print_if_while_statement buf "if" expr stmt ~indent

and print_if_else_statement
    (buf : Buffer.t)
    (expr: expression)
    (thenStmt: statement)
    (elseStmt: statement)
    ~(indent: int)
  =
  print_if_statement buf expr thenStmt ~indent;
  Buffer.add_string buf " else ";
  print_statement buf elseStmt ~indent

and print_while_statement
    (buf : Buffer.t)
    (expr: expression)
    (stmt: statement)
    ~(indent: int)
  =
  print_if_while_statement buf "while" expr stmt ~indent

let print_params (buf : Buffer.t) (params: identifiers) =
  Formatutil.print_separate_list buf params ~f: Buffer.add_string ~separator: ", "

let print_function (buf : Buffer.t) (name, params, stmts) =
  bprintf buf "function %s (%a) {\n%a\n}"
    name
    print_params params
    (print_statements ~indent: 2) stmts

let print_toplevel (buf : Buffer.t) (topl: toplevel) ~indent =
  match topl with
  | Statement stmt ->
    print_statement buf stmt ~indent
  | Function func ->
    print_function buf func

let print_ast (buf : Buffer.t) (ast: t) =
  Formatutil.print_statements buf ast ~f: print_toplevel ~indent: 0
