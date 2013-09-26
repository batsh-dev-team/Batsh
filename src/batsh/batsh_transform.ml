open Core.Std
open Batsh_ast

type t = {
  symtable : Symbol_table.t;
  split_string : bool;
  split_list_literal : bool;
  split_call : bool;
  split_string_compare : bool;
  split_arithmetic : bool;
}

let rec split_expression
    (expr : expression)
    ~(conf : t)
    ~(scope : Symbol_table.Scope.t)
    ~(subexpression : bool)
  : (statements * expression) =
  let split_expression = split_expression ~conf ~scope in
  let split_binary (left, right) ~subexpression =
    let assignments_left, left = split_expression left ~subexpression in
    let assignments_right, right = split_expression right ~subexpression in
    assignments_left @ assignments_right, (left, right)
  in
  let split_expr_to_assignment assignments expr : (statements * expression) =
    let ident = Symbol_table.Scope.add_temporary_variable scope in
    let variable = Identifier ident in
    let assignments = assignments @ [Assignment (variable, expr)] in
    assignments, (Leftvalue variable)
  in
  match expr with
  | Bool _ | Int _ | Float _ | Leftvalue _ ->
    [], expr
  | ArithUnary (operator, expr) ->
    let assignments, expr = split_expression expr ~subexpression: true in
    if subexpression && conf.split_arithmetic then
      split_expr_to_assignment assignments (ArithUnary (operator, expr))
    else
      assignments, ArithUnary (operator, expr)
  | ArithBinary (operator, left, right) ->
    let assignments, (left, right) = split_binary (left, right) ~subexpression: true in
    if subexpression && conf.split_arithmetic then
      split_expr_to_assignment assignments (ArithBinary (operator, left, right))
    else
      assignments, ArithBinary (operator, left, right)
  | String str ->
    if subexpression && conf.split_string then
      split_expr_to_assignment [] (String str)
    else
      [], (String str)
  | Concat (left, right) ->
    let assignments, (left, right) = split_binary (left, right) ~subexpression: false in
    if subexpression && conf.split_string then
      split_expr_to_assignment assignments (Concat (left, right))
    else
      assignments, (Concat (left, right))
  | StrCompare (operator, left, right) ->
    let assignments, (left, right) = split_binary (left, right) ~subexpression: false in
    if conf.split_string_compare || (subexpression && conf.split_string) then
      split_expr_to_assignment assignments (StrCompare (operator, left, right))
    else
      assignments, (StrCompare (operator, left, right))
  | Call (ident, exprs) ->
    let assignments, exprs = split_expressions exprs ~conf ~scope in
    if subexpression && conf.split_call then
      split_expr_to_assignment assignments (Call (ident, exprs))
    else
      assignments, (Call (ident, exprs))
  | List exprs ->
    let assignments, exprs = split_expressions exprs ~conf ~scope in
    if subexpression && conf.split_list_literal then
      split_expr_to_assignment assignments (List exprs)
    else
      assignments, (List exprs)

and split_expressions
    (exprs : expressions)
    ~(conf : t)
    ~(scope : Symbol_table.Scope.t)
  : (statements * expressions) =
  let assignments, exprs = List.fold exprs ~init: ([], [])
      ~f: (fun (assignments_acc, exprs_acc) expr ->
          let assignments, expr = split_expression expr
              ~conf
              ~scope
              ~subexpression: false
          in
          (assignments @ assignments_acc, expr :: exprs_acc)
        )
  in
  assignments, List.rev exprs

let rec split_statement
    (stmt : statement)
    ~(conf : t)
    ~(scope : Symbol_table.Scope.t)
  : statement =
  let prepend_assignments assignments stmt : statement =
    match assignments with
    | [] -> stmt
    | _ -> Block (assignments @ [stmt])
  in
  match stmt with
  | Empty | Global _ | Comment _ ->
    stmt
  | Expression expr ->
    let assignments, expr = split_expression expr ~conf ~scope ~subexpression: false in
    prepend_assignments assignments (Expression expr)
  | Assignment (lvalue, expr) ->
    let assignments, expr = split_expression expr ~conf ~scope ~subexpression: false in
    prepend_assignments assignments (Assignment (lvalue, expr))
  | If (expr, stmt) ->
    let assignments, expr = split_expression expr ~conf ~scope ~subexpression: true in
    let stmt = split_statement stmt ~conf ~scope in
    prepend_assignments assignments (If (expr, stmt))
  | IfElse (expr, then_stmt, else_stmt) ->
    let assignments, expr = split_expression expr ~conf ~scope ~subexpression: true in
    let then_stmt = split_statement then_stmt ~conf ~scope in
    let else_stmt = split_statement else_stmt ~conf ~scope in
    prepend_assignments assignments (IfElse (expr, then_stmt, else_stmt))
  | While (expr, stmt) ->
    let assignments, expr = split_expression expr ~conf ~scope ~subexpression: true in
    let stmt = split_statement stmt ~conf ~scope in
    prepend_assignments assignments (While (expr, stmt))
  | Block stmts ->
    Block (split_statements stmts ~conf ~scope)

and split_statements
    (stmts : statements)
    ~(conf : t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  List.map stmts ~f: (split_statement ~conf ~scope)

let split_function
    (name, params, stmts)
    ~(conf : t)
  =
  let scope = Symbol_table.scope conf.symtable name in
  let body = split_statements stmts ~conf ~scope in
  name, params, body

let split_toplevel
    (topl : toplevel)
    ~(conf : t)
  : toplevel =
  match topl with
  | Statement stmt ->
    Statement (split_statement stmt ~conf
                 ~scope: (Symbol_table.global_scope conf.symtable))
  | Function func ->
    Function (split_function func ~conf)

(* Split arithmetic expressions, list literal and command call *)
let split (ast : Batsh_ast.t) ~(conf : t) : Batsh_ast.t =
  List.map ast ~f: (split_toplevel ~conf)

let create
    (symtable : Symbol_table.t)
    ~(split_string : bool)
    ~(split_list_literal : bool)
    ~(split_call : bool)
    ~(split_string_compare : bool)
    ~(split_arithmetic : bool)
  : t =
  {
    symtable;
    split_string;
    split_list_literal;
    split_call;
    split_string_compare;
    split_arithmetic;
  }
