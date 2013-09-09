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
  | Equal of (arithmetic * arithmetic)
  | NotEqual of (arithmetic * arithmetic)
  | Greater of (arithmetic * arithmetic)
  | Less of (arithmetic * arithmetic)
  | GreaterEqual of (arithmetic * arithmetic)
  | LessEqual of (arithmetic * arithmetic)
  | Parentheses of arithmetic

type expression =
  | Variable of identifier
  | String of string
  | Result of arithmetic
  | Concat of (expression * expression)
  | Command of (expression * expressions)

and expressions = expression list

type statement = 
  | Let of (identifier * arithmetic)
  | Assignment of (identifier * expression)
  | Expression of expression
  | If of (expression * statements)
  | IfElse of (expression * statements * statements)
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
  | Statement.Equal (left, right) ->
      Equal (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.NotEqual (left, right) ->
      NotEqual (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.Greater (left, right) ->
      Greater (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.Less (left, right) ->
      Less (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.GreaterEqual (left, right) ->
      GreaterEqual (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.LessEqual (left, right) ->
      LessEqual (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.Parentheses expr ->
      Parentheses (compile_expr_to_arith expr)
  | Statement.List _ -> assert false
  | Statement.Concat _ -> assert false
  | Statement.Call _ -> assert false

let rec compile_expr (expr: Statement.expression) :expression =
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
        Concat (compile_expr left, compile_expr right)
    | Statement.Call (ident, exprs) ->
        let params = List.map exprs ~f: compile_expr in
        Command (String ident, params)
    | _ -> assert false

let rec compile_statement (stmt: Statement.statement) :statement =
  match stmt with
  | Statement.Assignment (ident, expr) ->
      if is_arith expr then
        Let (ident, compile_expr_to_arith expr)
      else
        Assignment (ident, compile_expr expr)
  | Statement.Expression expr ->
      Expression (compile_expr expr)
  | Statement.If (expr, stmt) ->
      compile_if_statement expr stmt
  | Statement.IfElse (expr, thenStmt, elseStmt) ->
      compile_if_else_statement expr thenStmt elseStmt
  | _ -> Empty (* TODO should be removed *)

and compile_block (stmt: Statement.statement) =
  match stmt with
  | Statement.Block [] ->
      [Empty]
  | Statement.Block stmts ->
      List.map stmts ~f: compile_statement
  | _ ->
      [compile_statement stmt]

and compile_if_statement (expr: Statement.expression) stmt :statement =
  If (compile_expr expr, compile_block stmt)

and compile_if_else_statement
    (expr: Statement.expression)
    (thenStmt: Statement.statement)
    (elseStmt: Statement.statement)
    :statement =
  IfElse (compile_expr expr, compile_block thenStmt, compile_block elseStmt)

let compile (program: Statement.statements) :statements =
  List.map program ~f: compile_statement

(*********** Output ***********)

let rec print_arith out (expr: arithmetic) =
  match expr with
  | Identifier identifier -> output_string out identifier
  | Int number -> output_string out (string_of_int number)
  | Float number -> output_string out (Float.to_string number)
  | Plus _ | Minus _ | Multiply _ | Divide _  | Modulo _
  | Equal _ | NotEqual _ | Greater _ | Less _ | GreaterEqual _ | LessEqual _ ->
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

let rec print_expression out (expr: expression) =
  match expr with
  | Variable var | Result Identifier var -> fprintf out "$%s" var
  | String str -> fprintf out "\"%s\"" str
  | Result arith ->
      fprintf out "$((%a))" print_arith arith
  | Concat (left, right) ->
      fprintf out "%a%a" print_expression left print_expression right
  | Command _ ->
      fprintf out "$(%a)" print_command expr

and print_command out (expr: expression) =
  match expr with
  | Command (ident, params) ->
    print_expression out ident;
    List.iter params ~f: (fun param ->
      output_string out " ";
      print_expression out param
    )
  | _ -> assert false

let print_indent out (indent: int) =
  output_string out (String.make indent ' ')

let rec print_statement out (stmt: statement) ~(indent: int) =
  match stmt with
  | Let (ident, arith) ->
      fprintf out "let \"%s = %a\"" ident print_arith arith
  | Assignment (ident, expr) ->
      fprintf out "%s=%a" ident print_expression expr
  | Expression (Command _ as expr) ->
      print_command out expr
  | Expression expr ->
      print_expression out expr
  | If (expr, stmts) ->
      print_if out expr stmts ~indent
  | IfElse (expr, then_stmts, else_stmts) ->
      print_if_else out expr then_stmts else_stmts ~indent
  | Empty ->
      output_string out "true"

and print_if out (expr: expression) (stmts: statements) ~(indent: int) =
  let print_statements_indented = print_statements ~indent: (indent + 2) in
  fprintf out "if [ %a ]; then\n%a%afi"
      print_expression expr print_statements_indented stmts print_indent indent

and print_if_else
  (out: out_channel)
  (expr: expression)
  (then_stmts: statements)
  (else_stmts: statements)
  ~(indent: int) =
  let print_statements_indented = print_statements ~indent: (indent + 2) in
  fprintf out "if [ %a ]; then\n%a%aelse\n%a%afi"
      print_expression expr
      print_statements_indented then_stmts
      print_indent indent
      print_statements_indented else_stmts
      print_indent indent

and print_statements out (stmts: statements) ~(indent: int) =
  List.iter stmts ~f: (fun stmt ->
    print_indent out indent;
    print_statement out stmt ~indent;
    output_string out "\n"
  )

let print (outx: out_channel) (program: statements) :unit =
  print_statements outx program ~indent: 0
