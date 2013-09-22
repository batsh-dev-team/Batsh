open Core.Std
open Bash_ast

module BAST = Batsh_ast
module Symbol_table = Batsh.Symbol_table

let rec is_arith (expr: BAST.expression) :bool =
  match expr with
  | BAST.String _
  | BAST.List _
  | BAST.StrBinary _
  | BAST.Call _ ->
    false
  | BAST.Bool _
  | BAST.Int _
  | BAST.Float _
  | BAST.Leftvalue _
  | BAST.ArithUnary _
  | BAST.ArithBinary _ ->
    true
  | BAST.Parentheses expr ->
    is_arith expr

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
  | BAST.Parentheses expr ->
    Parentheses (compile_expr_to_arith expr)
  | BAST.String _ 
  | BAST.List _
  | BAST.StrBinary _
  | BAST.Call _ ->
    let ident = Symbol_table.Scope.add_temporary_variable scope in
    ArithTemp (ident, compile_expr expr ~symtable ~scope)

and compile_expr
    (expr: BAST.expression)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :expression =
  if is_arith expr then
    Result (compile_expr_to_arith expr ~symtable ~scope)
  else
    let compile_expr = compile_expr ~symtable ~scope in
    match expr with
    | BAST.Bool false -> String "false"
    | BAST.Bool true -> String "true"
    | BAST.Int number -> String (string_of_int number)
    | BAST.Float number -> String (Float.to_string number)
    | BAST.String str -> String str
    | BAST.StrBinary (operator, left, right) ->
      compile_str_binary operator (compile_expr left) (compile_expr right)
        ~symtable ~scope
    | BAST.Call (ident, exprs) ->
      let params = List.map exprs ~f: compile_expr in
      Command (ident, params)
    | BAST.Parentheses expr ->
      compile_expr expr
    | BAST.List exprs ->
      List (List.map exprs ~f: compile_expr)
    | BAST.ArithUnary _
    | BAST.ArithBinary _
    | BAST.Leftvalue _ ->
      assert false

and compile_str_binary
    (operator: string)
    (left: expression)
    (right: expression)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :expression =
  match operator with
  | "++" ->
    StrBinary ("++", left, right)
  | "==" | "!=" ->
    let ident = Symbol_table.Scope.add_temporary_variable scope in
    StrTemp (ident, StrBinary (operator, left, right))
  | _ ->
    failwith ("Unknown operator: " ^ operator)

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

let rec extract_temporary_arith arith :(statements * arithmetic) =
  let extract_temporary_arith_binary (operator, left, right)
    :(statements * (string * arithmetic * arithmetic)) =
    let assignments_left, left = extract_temporary_arith left in
    let assignments_right, right = extract_temporary_arith right in
    (assignments_left @ assignments_right, (operator, left, right))
  in
  match arith with
  | ArithTemp (ident, expr) ->
    let variable = Identifier ident in
    let assignments, expr = extract_temporary_expr expr in
    let assignment = Assignment (variable, expr) in
    (assignment :: assignments, Leftvalue variable)
  | Int _ | Float _ | Leftvalue _ ->
    ([], arith)
  | Parentheses arith ->
    let assignments, arith = extract_temporary_arith arith in
    (assignments, Parentheses arith)
  | ArithUnary (operator, arith) ->
    let assignments, arith = extract_temporary_arith arith in
    (assignments, ArithUnary (operator, arith))
  | ArithBinary binary ->
    let assignments, binary = extract_temporary_arith_binary binary in
    (assignments, ArithBinary binary)

and extract_temporary_expr expr :(statements * expression) =
  let extract_temporary_exprs exprs =
    let assignments, exprs = List.fold exprs ~init: ([], [])
        ~f: (fun (assignments_acc, exprs_acc) expr ->
            let assignments, expr = extract_temporary_expr expr in
            (assignments @ assignments_acc, expr :: exprs_acc)
          )
    in
    (List.rev assignments, List.rev exprs)
  in
  match expr with
  | StrTemp (ident, test_expr) ->
    let variable = Identifier ident in
    let assignments, test_expr = extract_temporary_expr test_expr in
    let test_stmt = Expression test_expr in
    let assignment = Assignment (variable, Result (ArithUnary ("!", Leftvalue (Identifier "?")))) in
    (test_stmt :: assignment :: assignments, Variable variable)
  | String _ | Variable _ ->
    ([], expr)
  | Result arith ->
    let assignments, arith = extract_temporary_arith arith in
    (assignments, Result arith)
  | StrBinary (operator, left, right) ->
    let assignments_left, left = extract_temporary_expr left in
    let assignments_right, right = extract_temporary_expr right in
    (assignments_left @ assignments_right, StrBinary (operator, left, right))
  | Command (ident, exprs) ->
    let assignments, exprs = extract_temporary_exprs exprs in
    (assignments, Command (ident, exprs))
  | List exprs ->
    let assignments, exprs = extract_temporary_exprs exprs in
    (assignments, List exprs)

let rec extract_temporary stmt :(statements * statement) =
  match stmt with
  | Empty | Local _ | Comment _ ->
    ([], stmt)
  | Expression expr ->
    let assignments, expr = extract_temporary_expr expr in
    (assignments, Expression expr)
  | Assignment (lvalue, expr) ->
    let assignments, expr = extract_temporary_expr expr in
    (assignments, Assignment (lvalue, expr))
  | If (expr, stmt) ->
    let assignments, expr = extract_temporary_expr expr in
    (assignments, If (expr, stmt))
  | IfElse (expr, then_stmt, else_stmt) ->
    let assignments, expr = extract_temporary_expr expr in
    (assignments, IfElse (expr, then_stmt, else_stmt))
  | While (expr, stmt) ->
    let assignments, expr = extract_temporary_expr expr in
    (assignments, While (expr, stmt))
  | Block stmts ->
    let assignments, stmts = List.fold stmts ~init: ([], [])
        ~f: (fun (assignments_acc, stmts_acc) stmt ->
            let assignments, stmt = extract_temporary stmt in
            (assignments_acc @ assignments, stmt :: stmts_acc)
          )
    in
    (assignments, Block (List.rev stmts))

let rec compile_statement
    (stmt: BAST.statement)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :statement =
  let stmt = match stmt with
    | BAST.Comment comment ->
      Comment comment
    | BAST.Assignment (lvalue, expr) ->
      Assignment (compile_leftvalue lvalue ~symtable ~scope,
                  compile_expr expr ~symtable ~scope)
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
    | BAST.Empty ->
      Empty
  in
  let assignments, stmt = extract_temporary stmt in
  if List.length assignments = 0 then
    stmt
  else
    Block (List.concat [assignments; [stmt]])

and compile_if_statement
    (expr: BAST.expression)
    stmt
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :statement =
  If (compile_expr expr ~symtable ~scope, compile_statement stmt ~symtable ~scope)

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
  let param_locals = List.map params ~f: (fun ident -> Local ident) in
  let param_defines = List.mapi params ~f: (fun i param ->
      Assignment (Identifier param,
                  Variable (Identifier (string_of_int (i + 1))))
    )
  in
  Function (name, List.concat [locals; param_locals; param_defines; body])

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

let compile (batsh: Batsh.t) :asttype =
  let program = Batsh.ast batsh in
  let symtable = Batsh.symtable batsh in
  List.map program ~f: (compile_toplevel ~symtable)
