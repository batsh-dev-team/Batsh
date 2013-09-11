open Core.Std
open Bashast

let rec is_arith (expr: Statement.expression) :bool =
  match expr with
  | Statement.Bool _ | Statement.Int _ | Statement.Float _
  | Statement.Identifier _ ->
      true
  | Statement.String _ | Statement.List _ | Statement.Concat _ 
  | Statement.Call _ ->
      false
  | Statement.Parentheses expr ->
      is_arith expr
  | Statement.Plus (left, right)
  | Statement.Minus (left, right)
  | Statement.Multiply (left, right)
  | Statement.Divide (left, right)
  | Statement.Modulo (left, right)
  | Statement.Equal (left, right)
  | Statement.NotEqual (left, right)
  | Statement.Greater (left, right)
  | Statement.Less (left, right)
  | Statement.GreaterEqual (left, right)
  | Statement.LessEqual (left, right) ->
      is_arith left && is_arith right

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
      AEQ (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.NotEqual (left, right) ->
      ANE (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.Greater (left, right) ->
      AGT (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.Less (left, right) ->
      ALT (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.GreaterEqual (left, right) ->
      AGE (compile_expr_to_arith left, compile_expr_to_arith right)
  | Statement.LessEqual (left, right) ->
      ALE (compile_expr_to_arith left, compile_expr_to_arith right)
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
    | Statement.Equal (left, right) ->
        SEQ (compile_expr left, compile_expr right)
    | Statement.NotEqual (left, right) ->
        SNE (compile_expr left, compile_expr right)
    | Statement.Greater (left, right) ->
        SGT (compile_expr left, compile_expr right)
    | Statement.Less (left, right) ->
        SLT (compile_expr left, compile_expr right)
    | Statement.Parentheses expr ->
        compile_expr expr
    | Statement.List _ | Statement.Plus _ | Statement.Minus _
    | Statement.Multiply _ | Statement.Divide _ | Statement.Modulo _
    | Statement.GreaterEqual _ | Statement.LessEqual _ ->
        assert false

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
  | Statement.While (expr, stmt) ->
      compile_while_statement expr stmt
  | Statement.Block stmts -> assert false (* TODO *)
  | Statement.Empty ->
      Empty

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

and compile_while_statement (expr: Statement.expression) stmt :statement =
  While (compile_expr expr, compile_block stmt)

let compile (program: Statement.statements) :statements =
  List.map program ~f: compile_statement
