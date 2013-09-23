open Core.Std
open Bash_ast

let rec print_lvalue out (lvalue: leftvalue) ~(bare: bool) =
  match lvalue with
  | Identifier ident ->
    if not bare then
      output_string out "$";
    output_string out ident
  | ListAccess (lvalue, arith) ->
    let print_lvalue_bare = print_lvalue ~bare: true in
    if bare then
      fprintf out "%a[%a]" print_lvalue_bare lvalue print_arith arith
    else
      fprintf out "${%a[%a]}" print_lvalue_bare lvalue print_arith arith

and print_arith out (expr: arithmetic) =
  match expr with
  | Leftvalue lvalue -> print_lvalue out lvalue ~bare: false
  | Int number -> output_string out (string_of_int number)
  | Float number -> output_string out (Float.to_string number)
  | ArithUnary (operator, arith) ->
    fprintf out "%s%a" operator print_arith arith
  | ArithBinary binary ->
    print_arith_binary out binary
  | Parentheses expr ->
    fprintf out "(%a)" print_arith expr
  | ArithTemp _ ->
    failwith "BUG: ArithTemp should be all replaced."

and print_arith_binary
    (outx: out_channel)
    (operator, left, right)
  =
  let operator = match operator with
    | "===" -> "=="
    | "!==" -> "!="
    | _ -> operator
  in
  fprintf outx "%a %s %a" print_arith left operator print_arith right

let rec print_expression out (expr: expression) =
  match expr with
  | Variable lvalue | Result Leftvalue lvalue ->
    print_lvalue out lvalue ~bare: false
  | String str ->
    fprintf out "\"%s\"" (Formatutil.escape str)
  | Result arith ->
    fprintf out "$((%a))" print_arith arith
  | StrBinary binary ->
    print_str_binary out binary
  | Command cmd ->
    fprintf out "$(%a)" print_command cmd
  | List exprs ->
    output_string out "(";
    let num_exprs = List.length exprs in
    List.iteri exprs ~f: (fun i expr ->
        print_expression out expr;
        if i <> num_exprs - 1 then
          output_string out " "
      );
    output_string out ")"
  | StrTemp _ ->
    failwith "BUG: StrTemp should be all replaced."

and print_str_binary (outx: out_channel) (operator, left, right) =
  match operator with
  | "++" ->
    fprintf outx "%a%a" print_expression left print_expression right
  | "==" ->
    fprintf outx "[ %a == %a ]" print_expression left print_expression right
  | "!=" ->
    fprintf outx "[ %a != %a ]" print_expression left print_expression right
  | _ ->
    failwith ("Unknown operator: " ^ operator)

and print_command (outx: out_channel) (name, params) =
  fprintf outx "%a %a"
    print_expression name
    (Formatutil.print_separate_list ~f: print_expression ~separator: " ") params

let rec print_statement out (stmt: statement) ~(indent: int) =
  let () = match stmt with
    | Block _ -> ()
    | _ ->
      Formatutil.print_indent out indent in
  let print_lvalue = print_lvalue ~bare: true in
  match stmt with
  | Comment comment ->
    fprintf out "#%s" comment
  | Local ident ->
    fprintf out "local %s" ident
  | Assignment (lvalue, expr) ->
    fprintf out "%a=%a"
      print_lvalue lvalue
      print_expression expr
  | Expression (Command cmd) ->
    print_command out cmd
  | Expression expr ->
    print_expression out expr
  | If (expr, stmts) ->
    print_if out expr stmts ~indent
  | IfElse (expr, then_stmts, else_stmts) ->
    print_if_else out expr then_stmts else_stmts ~indent
  | While (expr, stmts) ->
    print_while out expr stmts ~indent
  | Block [] ->
    output_string out "-"
  | Block stmts ->
    print_statements out stmts ~indent
  | Empty -> ()

and print_condition (out: out_channel) (expr: expression) =
  fprintf out "[ %a == 1 ]" print_expression expr

and print_if_while
    (out: out_channel)
    (expr: expression)
    (stmt: statement)
    (first: string)
    (second: string)
    (third: string)
    ~(indent: int) =
  let print_statement_indented = print_statement ~indent: (indent + 2) in
  fprintf out "%s %a; %s\n%a%a\n%s"
    first (* if/while *)
    print_condition expr
    second (* then/do *)
    print_statement_indented stmt
    Formatutil.print_indent indent
    third (* fi/done *)

and print_if out (expr: expression) (stmt: statement) ~(indent: int) =
  print_if_while out expr stmt "if" "then" "fi" ~indent

and print_if_else
    (out: out_channel)
    (expr: expression)
    (then_stmt: statement)
    (else_stmt: statement)
    ~(indent: int) =
  let print_statement_indented = print_statement ~indent: (indent + 2) in
  fprintf out "if %a; then\n%a\n%aelse\n%a\n%afi"
    print_condition expr
    print_statement_indented then_stmt
    Formatutil.print_indent indent
    print_statement_indented else_stmt
    Formatutil.print_indent indent

and print_while out (expr: expression) (stmt: statement) ~(indent: int) =
  print_if_while out expr stmt "while" "do" "done" ~indent

and print_statements: out_channel -> statements -> indent:int -> unit =
  Formatutil.print_statements ~f: print_statement

let print_function (outx: out_channel) (name, stmts) =
  fprintf outx "function %s {\n%a\n}"
    name
    (print_statements ~indent: 2) stmts

let print_toplevel (outx: out_channel) (topl: toplevel) ~indent =
  match topl with
  | Statement stmt -> print_statement outx stmt ~indent
  | Function func -> print_function outx func

let print (outx: out_channel) (program: t) :unit =
  Formatutil.print_statements outx program ~f: print_toplevel ~indent: 0
