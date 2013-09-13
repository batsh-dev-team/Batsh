open Core.Std
open Bashast

let rec is_arith (expr: Batshast.expression) :bool =
  match expr with
  | Batshast.Bool _ | Batshast.Int _ | Batshast.Float _
  | Batshast.Leftvalue _ ->
    true
  | Batshast.String _ | Batshast.List _ | Batshast.Concat _ 
  | Batshast.Call _ ->
    false
  | Batshast.Parentheses expr ->
    is_arith expr
  | Batshast.Plus (left, right)
  | Batshast.Minus (left, right)
  | Batshast.Multiply (left, right)
  | Batshast.Divide (left, right)
  | Batshast.Modulo (left, right)
  | Batshast.Equal (left, right)
  | Batshast.NotEqual (left, right)
  | Batshast.Greater (left, right)
  | Batshast.Less (left, right)
  | Batshast.GreaterEqual (left, right)
  | Batshast.LessEqual (left, right) ->
    is_arith left && is_arith right

let rec compile_expr_to_arith (expr: Batshast.expression) :arithmetic =
  match expr with
  | Batshast.Bool false -> Int 0
  | Batshast.Bool true -> Int 1
  | Batshast.Int number -> Int number
  | Batshast.Float number -> Float number
  | Batshast.Leftvalue lvalue -> Leftvalue (compile_leftvalue lvalue)
  | Batshast.String str -> assert false
  | Batshast.Plus (left, right) ->
    Plus (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.Minus (left, right) ->
    Minus (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.Multiply (left, right) ->
    Multiply (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.Divide (left, right) ->
    Divide (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.Modulo (left, right) ->
    Modulo (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.Equal (left, right) ->
    AEQ (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.NotEqual (left, right) ->
    ANE (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.Greater (left, right) ->
    AGT (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.Less (left, right) ->
    ALT (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.GreaterEqual (left, right) ->
    AGE (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.LessEqual (left, right) ->
    ALE (compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.Parentheses expr ->
    Parentheses (compile_expr_to_arith expr)
  | Batshast.List _ -> assert false
  | Batshast.Concat _ -> assert false
  | Batshast.Call _ -> assert false

and compile_expr (expr: Batshast.expression) :expression =
  if is_arith expr then
    Result (compile_expr_to_arith expr)
  else
    match expr with
    | Batshast.Bool false -> String "false"
    | Batshast.Bool true -> String "true"
    | Batshast.Int number -> String (string_of_int number)
    | Batshast.Float number -> String (Float.to_string number)
    | Batshast.String str -> String str
    | Batshast.Concat (left, right) ->
      Concat (compile_expr left, compile_expr right)
    | Batshast.Call (ident, exprs) ->
      let params = List.map exprs ~f: compile_expr in
      Command (String ident, params)
    | Batshast.Equal (left, right) ->
      SEQ (compile_expr left, compile_expr right)
    | Batshast.NotEqual (left, right) ->
      SNE (compile_expr left, compile_expr right)
    | Batshast.Greater (left, right) ->
      SGT (compile_expr left, compile_expr right)
    | Batshast.Less (left, right) ->
      SLT (compile_expr left, compile_expr right)
    | Batshast.Parentheses expr ->
      compile_expr expr
    | Batshast.List exprs ->
      List (List.map exprs ~f: compile_expr)
    | Batshast.Plus _ | Batshast.Minus _
    | Batshast.Multiply _ | Batshast.Divide _ | Batshast.Modulo _
    | Batshast.GreaterEqual _ | Batshast.LessEqual _ | Batshast.Leftvalue _ ->
      assert false

and compile_leftvalue (lvalue: Batshast.leftvalue) :leftvalue =
  match lvalue with
  | Batshast.Identifier ident ->
    Identifier ident
  | Batshast.ListAccess (lvalue, expr) ->
    ListAccess (compile_leftvalue lvalue, compile_expr_to_arith expr)

let rec compile_statement (stmt: Batshast.statement) :statement =
  match stmt with
  | Batshast.Comment comment ->
    Comment comment
  | Batshast.Assignment (lvalue, expr) ->
    if is_arith expr then
      Let (compile_leftvalue lvalue, compile_expr_to_arith expr)
    else
      Assignment (compile_leftvalue lvalue, compile_expr expr)
  | Batshast.Expression expr ->
    Expression (compile_expr expr)
  | Batshast.If (expr, stmt) ->
    compile_if_statement expr stmt
  | Batshast.IfElse (expr, thenStmt, elseStmt) ->
    compile_if_else_statement expr thenStmt elseStmt
  | Batshast.While (expr, stmt) ->
    compile_while_statement expr stmt
  | Batshast.Block stmts ->
    Block (List.map stmts ~f: compile_statement)
  | Batshast.Empty ->
    Empty

and open_block (stmt: Batshast.statement) =
  match stmt with
  | Batshast.Block [] ->
    [Empty]
  | Batshast.Block stmts ->
    List.map stmts ~f: compile_statement
  | _ ->
    [compile_statement stmt]

and compile_if_statement (expr: Batshast.expression) stmt :statement =
  If (compile_expr expr, compile_statement stmt)

and compile_if_else_statement
    (expr: Batshast.expression)
    (thenStmt: Batshast.statement)
    (elseStmt: Batshast.statement)
  :statement =
  IfElse (compile_expr expr,
          compile_statement thenStmt,
          compile_statement elseStmt)

and compile_while_statement (expr: Batshast.expression) stmt :statement =
  While (compile_expr expr, compile_statement stmt)

let compile_statements (stmts: Batshast.statements) :statements =
  List.map stmts ~f: compile_statement

let compile (program: Batshast.statements) :statements =
  compile_statements program
