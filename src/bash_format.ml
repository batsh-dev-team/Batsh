open Core_kernel.Std
open Bash_ast

let rec print_lvalue_partial (buf : Buffer.t) (lvalue : leftvalue) =
  match lvalue with
  | Identifier ident ->
    Buffer.add_string buf ident
  | ListAccess (lvalue, arith) ->
    bprintf buf "%a[%a]"
      print_lvalue_partial lvalue
      (print_arith ~paren:false) arith
  | EntireList lvalue ->
    bprintf buf "%a[@]" print_lvalue_partial lvalue
  | Cardinal lvalue ->
    bprintf buf "#%a" print_lvalue_partial lvalue

and print_lvalue (buf : Buffer.t) (lvalue : leftvalue) ~(quote : bool) =
  let quote = if quote then "\"" else "" in
  match lvalue with
  | Identifier ident ->
    bprintf buf "%s$%a%s" quote print_lvalue_partial lvalue quote
  | ListAccess _
  | EntireList _
  | Cardinal _ ->
    bprintf buf "%s${%a}%s" quote print_lvalue_partial lvalue quote

and print_arith
    ?(paren = true)
    (buf : Buffer.t)
    (expr: arithmetic)
  =
  match expr with
  | Leftvalue lvalue -> print_lvalue buf lvalue ~quote:false
  | Int number -> Buffer.add_string buf (string_of_int number)
  | Float number -> Buffer.add_string buf (Float.to_string number)
  | ArithUnary (operator, arith) ->
    if paren then
      bprintf buf "%s(%a)" operator (print_arith ~paren:true) arith
    else
      bprintf buf "%s%a" operator (print_arith ~paren:true) arith
  | ArithBinary binary ->
    if paren then
      bprintf buf "(%a)" print_arith_binary binary
    else
      print_arith_binary buf binary

and print_arith_binary
    (buf : Buffer.t)
    (operator, left, right)
  =
  let operator = match operator with
    | "===" -> "=="
    | "!==" -> "!="
    | _ -> operator
  in
  bprintf buf "%a %s %a"
    (print_arith ~paren:true) left
    operator
    (print_arith ~paren:true) right

let rec print_expression buf (expr: expression) =
  match expr with
  | Variable lvalue | Result Leftvalue lvalue ->
    print_lvalue buf lvalue ~quote:true
  | String str ->
    bprintf buf "\"%s\"" (Formatutil.escape str)
  | Result arith ->
    bprintf buf "$((%a))" (print_arith ~paren:false) arith
  | StrBinary binary ->
    print_str_binary buf binary
  | TestUnary test ->
    print_test_unary buf test
  | Command cmd ->
    bprintf buf "$(%a)" print_command cmd
  | List exprs ->
    Buffer.add_string buf "(";
    let num_exprs = List.length exprs in
    List.iteri exprs ~f: (fun i expr ->
        print_expression buf expr;
        if i <> num_exprs - 1 then
          Buffer.add_string buf " "
      );
    Buffer.add_string buf ")"
  | Raw str ->
    Buffer.add_string buf str

and print_str_binary (buf: Buffer.t) (operator, left, right) =
  match operator with
  | "++" ->
    bprintf buf "%a%a" print_expression left print_expression right
  | "==" ->
    bprintf buf "[ %a == %a ]" print_expression left print_expression right
  | "!=" ->
    bprintf buf "[ %a != %a ]" print_expression left print_expression right
  | _ ->
    failwith ("Unknown operator: " ^ operator)

and print_test_unary (buf: Buffer.t) (operator, expr) =
  bprintf buf "[ %s %a ]" operator print_expression expr

and print_command (buf: Buffer.t) (name, params) =
  bprintf buf "%a %a"
    print_expression name
    (Formatutil.print_separate_list ~f: print_expression ~separator: " ") params

let rec print_statement buf (stmt: statement) ~(indent: int) =
  let () = match stmt with
    | Block _ -> ()
    | _ ->
      Formatutil.print_indent buf indent in
  match stmt with
  | Comment comment ->
    bprintf buf "#%s" comment
  | Local ident ->
    bprintf buf "local %s" ident
  | Assignment (lvalue, expr) ->
    bprintf buf "%a=%a"
      print_lvalue_partial lvalue
      print_expression expr
  | Expression (Command cmd) ->
    print_command buf cmd
  | Expression expr ->
    print_expression buf expr
  | If (expr, stmts) ->
    print_if buf expr stmts ~indent
  | IfElse (expr, then_stmts, else_stmts) ->
    print_if_else buf expr then_stmts else_stmts ~indent
  | While (expr, stmts) ->
    print_while buf expr stmts ~indent
  | Block [] ->
    Buffer.add_string buf "-"
  | Block stmts ->
    print_statements buf stmts ~indent
  | Return ->
    Buffer.add_string buf "return"
  | Empty -> ()

and print_condition (buf : Buffer.t) (expr : expression) =
  match expr with
  | StrBinary (("==", _, _) as bin)
  | StrBinary (("!=", _, _) as bin) ->
    print_str_binary buf bin
  | TestUnary test ->
    print_test_unary buf test
  | _ ->
    bprintf buf "[ %a == 1 ]" print_expression expr

and print_if_while
    (buf: Buffer.t)
    (expr: expression)
    (stmt: statement)
    (first: string)
    (second: string)
    (third: string)
    ~(indent: int) =
  let print_statement_indented = print_statement ~indent: (indent + 2) in
  bprintf buf "%s %a; %s\n%a%a\n%s"
    first (* if/while *)
    print_condition expr
    second (* then/do *)
    print_statement_indented stmt
    Formatutil.print_indent indent
    third (* fi/done *)

and print_if buf (expr: expression) (stmt: statement) ~(indent: int) =
  print_if_while buf expr stmt "if" "then" "fi" ~indent

and print_if_else
    (buf: Buffer.t)
    (expr: expression)
    (then_stmt: statement)
    (else_stmt: statement)
    ~(indent: int) =
  let print_statement_indented = print_statement ~indent: (indent + 2) in
  bprintf buf "if %a; then\n%a\n%aelse\n%a\n%afi"
    print_condition expr
    print_statement_indented then_stmt
    Formatutil.print_indent indent
    print_statement_indented else_stmt
    Formatutil.print_indent indent

and print_while buf (expr: expression) (stmt: statement) ~(indent: int) =
  print_if_while buf expr stmt "while" "do" "done" ~indent

and print_statements: Buffer.t -> statements -> indent:int -> unit =
  Formatutil.print_statements ~f: print_statement

let print_function (buf: Buffer.t) (name, stmts) =
  bprintf buf "function %s {\n%a\n}"
    name
    (print_statements ~indent: 2) stmts

let print_toplevel (buf: Buffer.t) (topl: toplevel) ~indent =
  match topl with
  | Statement stmt -> print_statement buf stmt ~indent
  | Function func -> print_function buf func

let print (buf: Buffer.t) (program: t) :unit =
  Formatutil.print_statements buf program ~f: print_toplevel ~indent: 0
