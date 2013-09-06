open Core.Std

(*********** Definition ***********)

type identifier = string

type arithmetic =
  | Identifier of identifier
  | Int of int
  | Float of float
  | Plus of (arithmetic * arithmetic)
  | Minus of (arithmetic * arithmetic)
  | Multiply of (arithmetic * arithmetic)
  | Divide of (arithmetic * arithmetic)
  | Modulo of (arithmetic * arithmetic)
  | Parentheses of arithmetic

type concatenation =
  | Variable of identifier
  | String of string
  | Result of arithmetic
  | Concat of (concatenation * concatenation)

type command = concatenation list

type statement = 
  | Let of (identifier * arithmetic)
  | Assignment of (identifier * concatenation)
  | Command of (concatenation * command)
  | Empty

and statements = statement list

(*********** Compilation ***********)

let rec is_arith (expr: Statement.expression) :bool =
  match expr with
  | Statement.String _ | Statement.List _ | Statement.Concat _ 
  | Statement.Call _ ->
      false
  | Statement.Plus (left, right) -> is_arith left && is_arith right
  | Statement.Minus (left, right) -> is_arith left && is_arith right
  | Statement.Multiply (left, right) -> is_arith left && is_arith right
  | Statement.Divide (left, right) -> is_arith left && is_arith right
  | Statement.Modulo (left, right) -> is_arith left && is_arith right
  | Statement.Parentheses expr -> is_arith expr
  | _ -> true

let rec compile_expr_to_arith (expr: Statement.expression) :arithmetic =
  match expr with
  | Statement.Bool false -> Int 0
  | Statement.Bool true -> Int 1
  | Statement.Int number -> Int number
  | Statement.Float number -> Float number
  | Statement.Identifier ident -> Identifier ident
  | Statement.String str -> assert false
  | Statement.Plus (left, right) ->
      Plus (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.Minus (left, right) ->
      Minus (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.Multiply (left, right) ->
      Multiply (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.Divide (left, right) ->
      Divide (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.Modulo (left, right) ->
      Modulo (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.Parentheses expr ->
      Parentheses (compile_expr_to_arith expr)
  | Statement.List _ -> assert false
  | Statement.Concat _ -> assert false
  | Statement.Call _ -> assert false

let rec compile_expr_to_concat (expr: Statement.expression) :concatenation =
  if is_arith expr then
    Result (compile_expr_to_arith expr)
  else
    match expr with
    | Statement.Bool false -> String "false"
    | Statement.Bool true -> String "true"
    | Statement.Int number -> String (string_of_int number)
    | Statement.Float number -> String (Float.to_string number)
    | Statement.String str -> String str
    | Statement.Identifier ident -> Variable ident
    | Statement.Concat (left, right) ->
        Concat (compile_expr_to_concat left, compile_expr_to_concat right)
    | _ -> assert false

let compile_statement (stmt: Statement.statement) :statement =
  match stmt with
  | Statement.Assignment (ident, expr) ->
      if is_arith expr then
        Let (ident, compile_expr_to_arith expr)
      else
        Assignment (ident, compile_expr_to_concat expr)
  | Statement.Expression Statement.Call (ident, exprs) ->
    let params = List.map exprs ~f: compile_expr_to_concat in
    Command (String ident, params)
  | _ -> Empty

let compile (program: Statement.statements) :statements =
  List.map program ~f: compile_statement

(*********** Output ***********)

let rec print_arith out (expr: arithmetic) =
  match expr with
  | Identifier identifier -> output_string out identifier
  | Int number -> output_string out (string_of_int number)
  | Float number -> output_string out (Float.to_string number)
  | Plus _ | Minus _ | Multiply _ | Divide _  | Modulo _ ->
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
  | _ -> assert false

let rec print_concat out (concat: concatenation) =
  match concat with
  | Variable var | Result Identifier var -> fprintf out "$%s" var
  | String str -> fprintf out "\"%s\"" str
  | Result arith ->
      fprintf out "$((%a))" print_arith arith
  | Concat (left, right) ->
      fprintf out "%a%a" print_concat left print_concat right

let print_indent out (indent: int) =
  output_string out (String.make indent ' ')

let rec print_statement out (stmt: statement) ~(indent: int) =
  match stmt with
  | Let (ident, arith) ->
      fprintf out "let \"%s = %a\"" ident print_arith arith
  | Assignment (ident, concat) ->
      fprintf out "%s = %a" ident print_concat concat
  | Command (ident, params) ->
      print_concat out ident;
      List.iter params ~f: (fun param ->
        output_string out " ";
        print_concat out param
      )
  | Empty -> ()

and print_statements out (stmts: statements) ~(indent: int) =
  List.iter stmts ~f: (fun stmt ->
    print_indent out indent;
    print_statement out stmt ~indent;
    output_string out "\n"
  )

let print (outx: out_channel) (program: statements) :unit =
  print_statements outx program ~indent: 0
