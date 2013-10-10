open Core.Std
open Batsh_ast

module Symbol_table = Batsh.Symbol_table

let rec split_expression
    ?(preserve_top = false)
    ?(split_call = true)
    (expr : expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
    ~(subexpression : bool)
  : (statement Dlist.t * expression) =
  let split_expression = split_expression ~symtable ~scope in
  let split_binary (left, right) ~subexpression =
    let assignments_left, left = split_expression left ~subexpression in
    let assignments_right, right = split_expression right ~subexpression in
    Dlist.append assignments_left assignments_right, (left, right)
  in
  let split_expr_to_assignment assignments expr : (statement Dlist.t * expression) =
    if preserve_top then
      assignments, expr
    else
      let ident = Symbol_table.Scope.add_temporary_variable scope in
      let variable = Identifier ident in
      let assignments = Dlist.append
          assignments
          (Dlist.of_list [Assignment (variable, expr)])
      in
      assignments, (Leftvalue variable)
  in
  match expr with
  | Bool _ | Int _ | Float _ | Leftvalue _ ->
    Dlist.empty (), expr
  | ArithUnary (operator, expr) ->
    let assignments, expr = split_expression expr ~subexpression: true in
    split_expr_to_assignment assignments (ArithUnary (operator, expr))
  | ArithBinary (operator, left, right) ->
    let assignments, (left, right) = split_binary (left, right) ~subexpression: true in
    split_expr_to_assignment assignments (ArithBinary (operator, left, right))
  | String str ->
    if subexpression then
      split_expr_to_assignment (Dlist.empty ()) (String str)
    else
      Dlist.empty (), (String str)
  | Concat (left, right) ->
    let assignments, (left, right) = split_binary (left, right) ~subexpression: false in
    if subexpression then
      split_expr_to_assignment assignments (Concat (left, right))
    else
      assignments, (Concat (left, right))
  | StrCompare (operator, left, right) ->
    let assignments, (left, right) = split_binary (left, right) ~subexpression: false in
    split_expr_to_assignment assignments (StrCompare (operator, left, right))
  | Call (ident, exprs) ->
    let assignments, exprs = split_expressions exprs ~symtable ~scope in
    if split_call then
      split_expr_to_assignment assignments (Call (ident, exprs))
    else
      assignments, (Call (ident, exprs))
  | List exprs ->
    let assignments, exprs = split_expressions exprs ~symtable ~scope in
    if subexpression then
      split_expr_to_assignment assignments (List exprs)
    else
      assignments, (List exprs)

and split_expressions
    (exprs : expressions)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : (statement Dlist.t * expressions) =
  let assignments, exprs = List.fold exprs ~init: (Dlist.empty (), [])
      ~f: (fun (assignments_acc, exprs_acc) expr ->
          let assignments, expr = split_expression expr
              ~symtable
              ~scope
              ~subexpression: false
          in
          (Dlist.append assignments assignments_acc, expr :: exprs_acc)
        )
  in
  assignments, List.rev exprs

let rec split_statement
    (stmt : statement)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statement =
  let prepend_assignments assignments stmt : statement =
    if Dlist.length assignments = 0 then
      stmt
    else
      Block (Dlist.to_list (Dlist.append assignments (Dlist.of_list [stmt])))
  in
  match stmt with
  | Empty | Global _ | Comment _ | Return None ->
    stmt
  | Expression expr ->
    let assignments, expr = split_expression expr ~symtable ~scope
      ~subexpression: false
      ~split_call: false
    in
    prepend_assignments assignments (Expression expr)
  | Return (Some expr) ->
    let assignments, expr = split_expression expr ~symtable ~scope ~subexpression: false in
    prepend_assignments assignments (Return (Some expr))
  | Assignment (lvalue, expr) ->
    let assignments, expr = split_expression expr ~symtable ~scope ~subexpression: false in
    prepend_assignments assignments (Assignment (lvalue, expr))
  | If (expr, stmt) ->
    let assignments, expr = split_expression expr
        ~symtable
        ~scope
        ~subexpression: true
        ~preserve_top: true
    in
    let stmt = split_statement stmt ~symtable ~scope in
    prepend_assignments assignments (If (expr, stmt))
  | IfElse (expr, then_stmt, else_stmt) ->
    let assignments, expr = split_expression expr
        ~symtable
        ~scope
        ~subexpression: true
        ~preserve_top: true
    in
    let then_stmt = split_statement then_stmt ~symtable ~scope in
    let else_stmt = split_statement else_stmt ~symtable ~scope in
    prepend_assignments assignments (IfElse (expr, then_stmt, else_stmt))
  | While (expr, stmt) ->
    let assignments, expr = split_expression expr
        ~symtable
        ~scope
        ~subexpression: true
        ~preserve_top: true
    in
    let stmt = split_statement stmt ~symtable ~scope in
    prepend_assignments assignments (While (expr, stmt))
  | Block stmts ->
    Block (split_statements stmts ~symtable ~scope)

and split_statements
    (stmts : statements)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  List.map stmts ~f: (split_statement ~symtable ~scope)

let split_function
    (name, params, stmts)
    ~(symtable : Symbol_table.t)
  =
  let scope = Symbol_table.scope symtable name in
  let body = split_statements stmts ~symtable ~scope in
  name, params, body

let split_toplevel
    (topl : toplevel)
    ~(symtable : Symbol_table.t)
  : toplevel =
  match topl with
  | Statement stmt ->
    Statement (split_statement stmt ~symtable
                 ~scope: (Symbol_table.global_scope symtable))
  | Function func ->
    Function (split_function func ~symtable)

(* Split arithmetic expressions, string literals, string comparisons,
   list literals, and command calls *)
let split (ast : Batsh_ast.t) ~(symtable : Symbol_table.t) : Batsh_ast.t =
  List.map ast ~f: (split_toplevel ~symtable)
