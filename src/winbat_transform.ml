open Core_kernel.Std
open Batsh_ast

let rec split_expression
    ?(no_split_top = false)
    ?(split_call = true)
    ?(split_arith = true)
    ?(split_string = false)
    ?(split_list = true)
    ?(split_primitive = false)
    (expr : expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : (statement Dlist.t * expression) =
  let split_binary (left, right) ~split_arith ~split_string =
    let assignments_left, left = split_expression left
        ~split_arith ~split_string ~symtable ~scope
    in
    let assignments_right, right = split_expression right
        ~split_arith ~split_string ~symtable ~scope
    in
    Dlist.append assignments_left assignments_right, (left, right)
  in
  let split_when ~cond current_assignments new_expr =
    let split_expr_to_assignment assignments expr
      : (statement Dlist.t * expression) =
      if no_split_top then
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
    if cond then
      split_expr_to_assignment current_assignments new_expr
    else
      current_assignments, new_expr
  in
  match expr with
  | Bool _ | Int _ | Float _ | Leftvalue _ ->
    split_when ~cond:split_primitive (Dlist.empty ()) expr
  | String str ->
    split_when ~cond:split_string (Dlist.empty ()) expr
  | ArithUnary (operator, expr) ->
    let split = match operator with
      | "!" -> true
      | _ -> false
    in
    let assignments, expr = split_expression expr ~symtable ~scope
        ~split_arith:split
        ~split_string:true
    in
    split_when ~cond:split_arith assignments (ArithUnary (operator, expr))
  | ArithBinary (operator, left, right) ->
    let split = match operator with
      | "===" | "!==" | ">" | "<" | ">=" | "<=" -> true
      | _ -> false
    in
    let assignments, (left, right) = split_binary (left, right)
        ~split_arith:split
        ~split_string:true
    in
    split_when ~cond:split_arith
      assignments (ArithBinary (operator, left, right))
  | Concat (left, right) ->
    let assignments, (left, right) = split_binary (left, right)
        ~split_arith:true
        ~split_string:false
    in
    split_when ~cond:split_string assignments (Concat (left, right))
  | StrCompare (operator, left, right) ->
    let assignments, (left, right) = split_binary (left, right)
        ~split_arith:true
        ~split_string:false
    in
    split_when ~cond:true assignments (StrCompare (operator, left, right))
  | Call (ident, exprs) ->
    (* If this is a function call, then split all its arguments *)
    let split_primitive = Symbol_table.is_function symtable ident in
    let assignments, exprs = split_expressions exprs
        ~split_primitive ~symtable ~scope
    in
    split_when ~cond:split_call assignments (Call (ident, exprs))
  | List exprs ->
    let assignments, exprs = split_expressions exprs
        ~split_primitive:false ~symtable ~scope
    in
    split_when ~cond:split_list assignments (List exprs)

and split_expressions
    (exprs : expressions)
    ~(split_primitive : bool)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : (statement Dlist.t * expressions) =
  let assignments, exprs = List.fold exprs ~init: (Dlist.empty (), [])
      ~f: (fun (assignments_acc, exprs_acc) expr ->
          let assignments, expr = split_expression expr
              ~split_string:split_primitive
              ~split_primitive
              ~symtable ~scope
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
        ~split_call:false
    in
    prepend_assignments assignments (Expression expr)
  | Return (Some expr) ->
    let assignments, expr = split_expression expr ~symtable ~scope in
    prepend_assignments assignments (Return (Some expr))
  | Assignment (lvalue, expr) ->
    let assignments, expr = split_expression expr
        ~symtable
        ~scope
        ~split_arith:false
        ~split_call:false
        ~split_list:false
    in
    prepend_assignments assignments (Assignment (lvalue, expr))
  | If (expr, stmt) ->
    let assignments, expr = split_expression expr
        ~symtable
        ~scope
        ~no_split_top:true
    in
    let stmt = split_statement stmt ~symtable ~scope in
    prepend_assignments assignments (If (expr, stmt))
  | IfElse (expr, then_stmt, else_stmt) ->
    let assignments, expr = split_expression expr
        ~symtable
        ~scope
        ~no_split_top:true
    in
    let then_stmt = split_statement then_stmt ~symtable ~scope in
    let else_stmt = split_statement else_stmt ~symtable ~scope in
    prepend_assignments assignments (IfElse (expr, then_stmt, else_stmt))
  | While (expr, stmt) ->
    let assignments, expr = split_expression expr
        ~symtable
        ~scope
        ~no_split_top:true
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
