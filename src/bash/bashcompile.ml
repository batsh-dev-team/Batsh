open Core.Std
open Bashast

let rec is_arith (expr: Batshast.expression) :bool =
  match expr with
  | Batshast.String _
  | Batshast.List _
  | Batshast.StrBinary _
  | Batshast.Call _ ->
    false
  | Batshast.Bool _
  | Batshast.Int _
  | Batshast.Float _
  | Batshast.Leftvalue _
  | Batshast.ArithBinary _ ->
    true
  | Batshast.Parentheses expr ->
    is_arith expr

let rec compile_expr_to_arith
    (expr: Batshast.expression)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.scope_type)
  :arithmetic =
  let compile_expr_to_arith = compile_expr_to_arith ~symtable ~scope in
  match expr with
  | Batshast.Bool false -> Int 0
  | Batshast.Bool true -> Int 1
  | Batshast.Int number -> Int number
  | Batshast.Float number -> Float number
  | Batshast.Leftvalue lvalue ->
    Leftvalue (compile_leftvalue lvalue ~symtable ~scope)
  | Batshast.ArithBinary (operator, left, right) ->
    ArithBinary (operator, compile_expr_to_arith left, compile_expr_to_arith right)
  | Batshast.Parentheses expr ->
    Parentheses (compile_expr_to_arith expr)
  | Batshast.String _ 
  | Batshast.List _
  | Batshast.StrBinary _
  | Batshast.Call _ ->
    let ident = Symbol_table.add_temporary_variable symtable ~scope in
    Temporary (ident, compile_expr expr ~symtable ~scope)

and compile_expr
    (expr: Batshast.expression)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.scope_type)
  :expression =
  if is_arith expr then
    Result (compile_expr_to_arith expr ~symtable ~scope)
  else
    let compile_expr = compile_expr ~symtable ~scope in
    match expr with
    | Batshast.Bool false -> String "false"
    | Batshast.Bool true -> String "true"
    | Batshast.Int number -> String (string_of_int number)
    | Batshast.Float number -> String (Float.to_string number)
    | Batshast.String str -> String str
    | Batshast.StrBinary (operator, left, right) ->
      StrBinary (operator, compile_expr left, compile_expr right)
    | Batshast.Call (ident, exprs) ->
      let params = List.map exprs ~f: compile_expr in
      Command (ident, params)
    | Batshast.Parentheses expr ->
      compile_expr expr
    | Batshast.List exprs ->
      List (List.map exprs ~f: compile_expr)
    | Batshast.ArithBinary _
    | Batshast.Leftvalue _ ->
      assert false

and compile_leftvalue
    (lvalue: Batshast.leftvalue)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.scope_type)
  :leftvalue =
  match lvalue with
  | Batshast.Identifier ident ->
    Identifier ident
  | Batshast.ListAccess (lvalue, expr) ->
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
  | Temporary (ident, expr) ->
    let variable = Identifier ident in
    let assignments, expr = extract_temporary_expr expr in
    let assignment = Assignment (variable, expr) in
    (assignment :: assignments, Leftvalue variable)
  | Int _ | Float _ | Leftvalue _ ->
    ([], arith)
  | Parentheses arith ->
    let assignments, arith = extract_temporary_arith arith in
    (assignments, Parentheses arith)
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
    (stmt: Batshast.statement)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.scope_type)
  :statement =
  let stmt = match stmt with
    | Batshast.Comment comment ->
      Comment comment
    | Batshast.Assignment (lvalue, expr) ->
      Assignment (compile_leftvalue lvalue ~symtable ~scope,
                  compile_expr expr ~symtable ~scope)
    | Batshast.Expression expr ->
      Expression (compile_expr expr ~symtable ~scope)
    | Batshast.If (expr, stmt) ->
      compile_if_statement expr stmt ~symtable ~scope
    | Batshast.IfElse (expr, thenStmt, elseStmt) ->
      compile_if_else_statement expr thenStmt elseStmt ~symtable ~scope
    | Batshast.While (expr, stmt) ->
      compile_while_statement expr stmt ~symtable ~scope
    | Batshast.Block stmts ->
      Block (List.map stmts ~f: (compile_statement ~symtable ~scope))
    | Batshast.Global _ ->
      Empty
    | Batshast.Empty ->
      Empty
  in
  let assignments, stmt = extract_temporary stmt in
  if List.length assignments = 0 then
    stmt
  else
    Block (List.concat [assignments; [stmt]])

and compile_if_statement
    (expr: Batshast.expression)
    stmt
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.scope_type)
  :statement =
  If (compile_expr expr ~symtable ~scope, compile_statement stmt ~symtable ~scope)

and compile_if_else_statement
    (expr: Batshast.expression)
    (thenStmt: Batshast.statement)
    (elseStmt: Batshast.statement)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.scope_type)
  :statement =
  IfElse (compile_expr expr ~symtable ~scope,
          compile_statement thenStmt ~symtable ~scope,
          compile_statement elseStmt ~symtable ~scope)

and compile_while_statement
    (expr: Batshast.expression)
    stmt
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.scope_type)
  :statement =
  While (compile_expr expr ~symtable ~scope,
         compile_statement stmt ~symtable ~scope)

let compile_statements
    (stmts: Batshast.statements)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.scope_type)
  :statements =
  List.map stmts ~f: (compile_statement ~symtable ~scope)

let compile_function
    (name, params, stmts)
    ~(symtable: Symbol_table.t)
  :toplevel =
  let scope = Symbol_table.FunctionScope name in
  let body = compile_statements stmts ~symtable ~scope in
  let locals = Symbol_table.fold_variables symtable
      ~scope
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
    (topl: Batshast.toplevel)
  :toplevel =
  match topl with
  | Batshast.Statement stmt ->
    Statement (compile_statement stmt ~symtable
                 ~scope: Symbol_table.GlobalScope)
  | Batshast.Function func ->
    compile_function func ~symtable

let compile (program: Batshast.asttype) :asttype =
  let symtable = Symbol_table.create program in
  List.map program ~f: (compile_toplevel ~symtable)
