open Core_kernel.Std
open Bash_ast

module BAST = Batsh_ast

let is_arith (expr: BAST.expression) :bool =
  match expr with
  | BAST.String _
  | BAST.List _
  | BAST.StrCompare _
  | BAST.Concat _
  | BAST.Call _ ->
    false
  | BAST.Bool _
  | BAST.Int _
  | BAST.Float _
  | BAST.Leftvalue _
  | BAST.ArithUnary _
  | BAST.ArithBinary _ ->
    true

let is_leftvalue (expr : BAST.expression) : bool =
  match expr with
  | BAST.Leftvalue _ -> true
  | _ -> false

let rec compile_expr_to_arith
    (expr: BAST.expression)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :arithmetic =
  let compile_expr_to_arith = compile_expr_to_arith ~symtable ~scope in
  match expr with
  | BAST.Bool false -> Int 0
  | BAST.Bool true -> Int 1
  | BAST.Int number -> Int number
  | BAST.Float number -> Float number
  | BAST.Leftvalue lvalue ->
    Leftvalue (compile_leftvalue lvalue ~symtable ~scope)
  | BAST.ArithUnary (operator, expr) ->
    ArithUnary (operator, compile_expr_to_arith expr)
  | BAST.ArithBinary (operator, left, right) ->
    ArithBinary (operator,
                 compile_expr_to_arith left,
                 compile_expr_to_arith right)
  | BAST.String _
  | BAST.List _
  | BAST.StrCompare _
  | BAST.Concat _
  | BAST.Call _ ->
    assert false

and compile_expr
    (expr: BAST.expression)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  : expression =
  if is_arith expr && not (is_leftvalue expr) then
    Result (compile_expr_to_arith expr ~symtable ~scope)
  else
    let compile_expr = compile_expr ~symtable ~scope in
    match expr with
    | BAST.Bool false -> String "false"
    | BAST.Bool true -> String "true"
    | BAST.Int number -> String (string_of_int number)
    | BAST.Float number -> String (Float.to_string number)
    | BAST.String str -> String str
    | BAST.Leftvalue lvalue ->
      Variable (compile_leftvalue lvalue ~symtable ~scope)
    | BAST.StrCompare (operator, left, right) ->
      StrBinary (operator,
                 compile_expr left,
                 compile_expr right)
    | BAST.Concat (left, right) ->
      StrBinary ("++",
                 compile_expr left,
                 compile_expr right)
    | BAST.Call (ident, exprs) ->
      compile_call (ident, exprs) ~symtable ~scope
    | BAST.List exprs ->
      List (List.map exprs ~f: compile_expr)
    | BAST.ArithUnary _
    | BAST.ArithBinary _ ->
      assert false

and compile_call
    (ident, exprs)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  : expression =
  match ident with
  | "exists" ->
    let params_1 params =
      match params with
      | param :: _ -> param
      | _ -> failwith ("exists must have only 1 parameter.")
    in
    let param = compile_expr (params_1 exprs) ~symtable ~scope in
    TestUnary ("-e", param)
  | _ ->
    let params = List.map exprs ~f: (compile_expr ~symtable ~scope) in
    Command (String ident, params)

and compile_leftvalue
    (lvalue: BAST.leftvalue)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :leftvalue =
  match lvalue with
  | BAST.Identifier ident ->
    Identifier ident
  | BAST.ListAccess (lvalue, expr) ->
    ListAccess (compile_leftvalue lvalue ~symtable ~scope,
                compile_expr_to_arith expr ~symtable ~scope)

let rec compile_statement
    (stmt: BAST.statement)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :statement =
  match stmt with
  | BAST.Comment comment ->
    Comment comment
  | BAST.Assignment assignment ->
    compile_assignment assignment ~symtable ~scope
  | BAST.Expression expr ->
    Expression (compile_expr expr ~symtable ~scope)
  | BAST.If (expr, stmt) ->
    compile_if_statement expr stmt ~symtable ~scope
  | BAST.IfElse (expr, thenStmt, elseStmt) ->
    compile_if_else_statement expr thenStmt elseStmt ~symtable ~scope
  | BAST.While (expr, stmt) ->
    compile_while_statement expr stmt ~symtable ~scope
  | BAST.Block stmts ->
    Block (List.map stmts ~f: (compile_statement ~symtable ~scope))
  | BAST.Global _ ->
    Empty
  | BAST.Return (Some expr) ->
    let call_stmt = BAST.Expression (BAST.Call ("print", [expr])) in
    Block [compile_statement call_stmt ~symtable ~scope; Return]
  | BAST.Return None ->
    Return
  | BAST.Empty ->
    Empty

and compile_assignment
    (lvalue, expr)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  : statement =
  let lvalue = compile_leftvalue lvalue ~symtable ~scope in
  let expr_compiled = compile_expr expr ~symtable ~scope in
  let split_test (test_stmt : statement) : statement =
    let assignment = Assignment
        (lvalue,
         Result (
           ArithUnary ("!",
                       Leftvalue (Identifier "?"))))
    in
    Block [test_stmt; assignment]
  in
  match expr with
  | BAST.StrCompare _ ->
    let test_stmt = Expression expr_compiled in
    split_test test_stmt
  | BAST.Call (("exists", _) as call) ->
    let test_expr = compile_call call ~symtable ~scope in
    split_test (Expression test_expr)
  | _ ->
    Assignment (lvalue, expr_compiled)

and compile_if_statement
    (expr: BAST.expression)
    stmt
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :statement =
  If (compile_expr expr ~symtable ~scope,
      compile_statement stmt ~symtable ~scope)

and compile_if_else_statement
    (expr: BAST.expression)
    (thenStmt: BAST.statement)
    (elseStmt: BAST.statement)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :statement =
  IfElse (compile_expr expr ~symtable ~scope,
          compile_statement thenStmt ~symtable ~scope,
          compile_statement elseStmt ~symtable ~scope)

and compile_while_statement
    (expr: BAST.expression)
    stmt
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :statement =
  While (compile_expr expr ~symtable ~scope,
         compile_statement stmt ~symtable ~scope)

let compile_statements
    (stmts: BAST.statements)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :statements =
  List.map stmts ~f: (compile_statement ~symtable ~scope)

let compile_function
    (name, params, stmts)
    ~(symtable: Symbol_table.t)
  :toplevel =
  let scope = Symbol_table.scope symtable name in
  let body = compile_statements stmts ~symtable ~scope in
  let locals = Symbol_table.Scope.fold scope
      ~init: []
      ~f: (fun ident global acc ->
          if global then
            acc
          else
            (Local ident) :: acc
        )
  in
  let param_defines = List.mapi params ~f: (fun i param ->
      Assignment (Identifier param,
                  Variable (Identifier (string_of_int (i + 1))))
    )
  in
  Function (name, List.concat [locals; param_defines; body])

let compile_toplevel
    ~(symtable: Symbol_table.t)
    (topl: BAST.toplevel)
  :toplevel =
  match topl with
  | BAST.Statement stmt ->
    Statement (compile_statement stmt ~symtable
                 ~scope: (Symbol_table.global_scope symtable))
  | BAST.Function func ->
    compile_function func ~symtable

let compile (batsh : Parser.t) : t =
  let symtable = Parser.symtable batsh in
  let program = Bash_transform.split (Parser.ast batsh) ~symtable in
  List.map program ~f: (compile_toplevel ~symtable)
