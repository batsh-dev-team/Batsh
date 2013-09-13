open Core.Std
open Bashast

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
  | Leftvalue lvalue -> print_lvalue out lvalue ~bare: true
  | Int number -> output_string out (string_of_int number)
  | Float number -> output_string out (Float.to_string number)
  | Plus _ | Minus _ | Multiply _ | Divide _  | Modulo _
  | AEQ _ | ANE _ | AGT _ | ALT _ | AGE _ | ALE _ ->
    print_binary_arith out expr
  | Parentheses expr ->
    fprintf out "(%a)" print_arith expr

and print_binary_arith out (expr: arithmetic) =
  let print_binary operator left right =
    fprintf out "%a %s %a" print_arith left operator print_arith right
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
  | AEQ (left, right) ->
    print_binary "==" left right
  | ANE (left, right) ->
    print_binary "!=" left right
  | AGT (left, right) ->
    print_binary ">" left right
  | ALT (left, right) ->
    print_binary "<" left right
  | AGE (left, right) ->
    print_binary ">=" left right
  | ALE (left, right) ->
    print_binary "<=" left right
  | _ -> assert false

let rec print_expression out (expr: expression) =
  match expr with
  | Variable lvalue | Result Leftvalue lvalue ->
    print_lvalue out lvalue ~bare: false
  | String str -> fprintf out "\"%s\"" str
  | Result arith ->
    fprintf out "$((%a))" print_arith arith
  | Concat (left, right) ->
    fprintf out "%a%a" print_expression left print_expression right
  | Command _ ->
    fprintf out "$(%a)" print_command expr
  | SEQ (left, right) ->
    fprintf out "[ %a == %a ]" print_expression left print_expression right
  | SNE (left, right) ->
    fprintf out "[ %a != %a ]" print_expression left print_expression right
  | SGT (left, right) ->
    fprintf out "[ %a > %a ]" print_expression left print_expression right
  | SLT (left, right) ->
    fprintf out "[ %a < %a ]" print_expression left print_expression right
  | List exprs ->
    output_string out "(";
    let num_exprs = List.length exprs in
    List.iteri exprs ~f: (fun i expr ->
        print_expression out expr;
        if i <> num_exprs - 1 then
          output_string out " "
      );
    output_string out ")"

and print_command out (expr: expression) =
  match expr with
  | Command (ident, params) ->
    print_expression out ident;
    List.iter params ~f: (fun param ->
        output_string out " ";
        print_expression out param
      )
  | _ -> assert false

let rec print_statement out (stmt: statement) ~(indent: int) =
  let print_lvalue = print_lvalue ~bare: true in
  match stmt with
  | Comment comment ->
    fprintf out "#%s" comment
  | Let (lvalue, arith) ->
    fprintf out "let \"%a = %a\""
      print_lvalue lvalue
      print_arith arith
  | Assignment (lvalue, expr) ->
    fprintf out "%a=%a"
      print_lvalue lvalue
      print_expression expr
  | Expression (Command _ as expr) ->
    print_command out expr
  | Expression expr ->
    print_expression out expr
  | If (expr, stmts) ->
    print_if out expr stmts ~indent
  | IfElse (expr, then_stmts, else_stmts) ->
    print_if_else out expr then_stmts else_stmts ~indent
  | While (expr, stmts) ->
    print_while out expr stmts ~indent
  | Empty ->
    output_string out "true"
  | Block stmts ->
    print_statements out stmts ~indent

and print_condition (out: out_channel) (expr: expression) =
  match expr with
  | SEQ _ | SNE _ | SGT _ | SLT _ ->
    print_expression out expr
  | _ ->
    fprintf out "[ %a == 1 ]" print_expression expr

and print_if_while
    (out: out_channel)
    (expr: expression)
    (stmts: statements)
    (first: string)
    (second: string)
    (third: string)
    ~(indent: int) =
  let print_statements_indented = print_statements ~indent: (indent + 2) in
  fprintf out "%s %a; %s\n%a%a\n%s"
    first (* if/while *)
    print_condition expr
    second (* then/do *)
    print_statements_indented stmts
    Formatutil.print_indent indent
    third (* fi/done *)

and print_if out (expr: expression) (stmts: statements) ~(indent: int) =
  print_if_while out expr stmts "if" "then" "fi" ~indent

and print_if_else
    (out: out_channel)
    (expr: expression)
    (then_stmts: statements)
    (else_stmts: statements)
    ~(indent: int) =
  let print_statements_indented = print_statements ~indent: (indent + 2) in
  fprintf out "if %a; then\n%a\n%aelse\n%a\n%afi"
    print_condition expr
    print_statements_indented then_stmts
    Formatutil.print_indent indent
    print_statements_indented else_stmts
    Formatutil.print_indent indent

and print_while out (expr: expression) (stmts: statements) ~(indent: int) =
  print_if_while out expr stmts "while" "do" "done" ~indent

and print_statements: out_channel -> statements -> indent:int -> unit =
  Formatutil.print_statements ~f: print_statement

let print (outx: out_channel) (program: statements) :unit =
  print_statements outx program ~indent: 0
