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

and compile_expr
    (expr: Batshast.expression)
    ~(symtable: Symbol_table.t)
  :expression =
  if is_arith expr then
    Result (compile_expr_to_arith expr)
  else
    let compile_expr = compile_expr ~symtable in
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

let rec compile_statement
    (stmt: Batshast.statement)
    ~(symtable: Symbol_table.t)
  :statement =
  match stmt with
  | Batshast.Comment comment ->
    Comment comment
  | Batshast.Assignment (lvalue, expr) ->
    let print_let = is_arith expr && match expr with
      (* if right value is only a left value, do not use let*)
      | Batshast.Leftvalue _ -> false
      | _ -> true in
    if print_let then
      Let (compile_leftvalue lvalue, compile_expr_to_arith expr)
    else
      Assignment (compile_leftvalue lvalue, compile_expr expr ~symtable)
  | Batshast.Expression expr ->
    Expression (compile_expr expr ~symtable)
  | Batshast.If (expr, stmt) ->
    compile_if_statement expr stmt ~symtable
  | Batshast.IfElse (expr, thenStmt, elseStmt) ->
    compile_if_else_statement expr thenStmt elseStmt ~symtable
  | Batshast.While (expr, stmt) ->
    compile_while_statement expr stmt ~symtable
  | Batshast.Block stmts ->
    Block (List.map stmts ~f: (compile_statement ~symtable))
  | Batshast.Global _ ->
    Empty
  | Batshast.Empty ->
    Empty

and compile_if_statement
    (expr: Batshast.expression)
    stmt
    ~(symtable: Symbol_table.t)
  :statement =
  If (compile_expr expr ~symtable, compile_statement stmt ~symtable)

and compile_if_else_statement
    (expr: Batshast.expression)
    (thenStmt: Batshast.statement)
    (elseStmt: Batshast.statement)
    ~(symtable: Symbol_table.t)
  :statement =
  IfElse (compile_expr expr ~symtable,
          compile_statement thenStmt ~symtable,
          compile_statement elseStmt ~symtable)

and compile_while_statement
    (expr: Batshast.expression)
    stmt
    ~(symtable: Symbol_table.t)
  :statement =
  While (compile_expr expr ~symtable, compile_statement stmt ~symtable)

let compile_statements
    (stmts: Batshast.statements)
    ~(symtable: Symbol_table.t)
  :statements =
  List.map stmts ~f: (compile_statement ~symtable)

let compile_function
    (name, params, stmts)
    ~(symtable: Symbol_table.t)
  :toplevel =
  let ident_local ident = Local ident in
  let locals = Symbol_table.fold_variables symtable ~scope: (Some name)
      ~init: []
      ~f: (fun ident global acc ->
          if global then
            acc
          else
            (Local ident) :: acc
        )
  in
  let param_locals = List.map params ~f: ident_local in
  let param_defines = List.mapi params ~f: (fun i param ->
      Assignment (Identifier param,
                  Variable (Identifier (string_of_int (i + 1))))
    )
  in
  let body = compile_statements stmts ~symtable in
  Function (name, List.concat [locals; param_locals; param_defines; body])

let compile_toplevel
    ~(symtable: Symbol_table.t)
    (topl: Batshast.toplevel)
  :toplevel =
  match topl with
  | Batshast.Statement stmt ->
    Statement (compile_statement stmt ~symtable)
  | Batshast.Function func ->
    compile_function func ~symtable

let compile (program: Batshast.asttype) :asttype =
  let symtable = Symbol_table.create program in
  List.map program ~f: (compile_toplevel ~symtable)
