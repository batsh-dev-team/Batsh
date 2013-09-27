open Core.Std
open Batsh_ast
open Winbat_ast

module Symbol_table = Batsh.Symbol_table

let rec compile_leftvalue
    (lvalue: Batsh_ast.leftvalue)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  : leftvalue =
  match lvalue with
  | Identifier ident ->
    `Identifier ident
  | ListAccess (lvalue, index) ->
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    let index = compile_expression_to_varint index ~symtable ~scope in
    `ListAccess (lvalue, index)

and compile_expression_to_varint
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : varint =
  match expr with
  | Leftvalue lvalue ->
    `Var (compile_leftvalue lvalue ~symtable ~scope)
  | Int num ->
    `Int num
  | _ ->
    failwith "Index should be either var or int"

let rec compile_expression_to_arith
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : arithmetic =
  match expr with
  | Bool false ->
    `Int 0
  | Bool true ->
    `Int 1
  | Int num ->
    `Int num
  | Leftvalue lvalue ->
    `Var (compile_leftvalue lvalue ~symtable ~scope)
  | ArithUnary (operator, expr) ->
    `ArithUnary (operator, compile_expression_to_arith expr ~symtable ~scope)
  | ArithBinary (operator, left, right) ->
    `ArithBinary (operator,
                  compile_expression_to_arith left ~symtable ~scope,
                  compile_expression_to_arith right ~symtable ~scope)
  | String _
  | Float _
  | List _
  | Concat _
  | StrCompare _
  | Call _ ->
    failwith "Can not be here"

let rec compile_expression
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : varstrings =
  match expr with
  | Bool false ->
    [`Str "0"]
  | Bool true ->
    [`Str "1"]
  | Int num ->
    [`Str (string_of_int num)]
  | String str ->
    [`Str str]
  | Leftvalue lvalue ->
    [`Var (compile_leftvalue lvalue ~symtable ~scope)]
  | Concat (left, right) ->
    let left = compile_expression left ~symtable ~scope in
    let right = compile_expression right ~symtable ~scope in
    left @ right
  | Call _ ->
    failwith "Not implemented: get stdout of given command"
  | _ ->
    assert false (* TODO *)

let compile_expressions
    (exprs : Batsh_ast.expressions)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : varstrings =
  List.concat (List.map exprs ~f: (compile_expression ~symtable ~scope))

let compile_expression_to_comparison
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : comparison =
  match expr with
  | StrCompare (operator, left, right)
  | ArithBinary (operator, left, right) ->
    let left = compile_expression left ~symtable ~scope in
    let right = compile_expression right ~symtable ~scope in
    `StrCompare (operator, left, right)
  | Leftvalue lvalue ->
    let lvalue = `Var (compile_leftvalue lvalue ~symtable ~scope) in
    `StrCompare ("==", [lvalue], [`Str "1"])
  | Bool true | Int 1 ->
    `StrCompare ("==", [`Str "1"], [`Str "1"])
  | Bool false | Int _ ->
    `StrCompare ("==", [`Str "0"], [`Str "1"])
  | _ ->
    failwith "Expression can not compile to comparison"

let rec compile_expression_statement
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statement =
  match expr with
  | Call (ident, exprs) ->
    `Call (`Str ident, compile_expressions exprs ~symtable ~scope)
  | _ ->
    assert false (* TODO *)

let rec compile_statement
    (stmt : Batsh_ast.statement)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  match stmt with
  | Comment comment ->
    [`Comment comment]
  | Block stmts ->
    compile_statements stmts ~symtable ~scope
  | Expression expr ->
    [compile_expression_statement expr ~symtable ~scope]
  | Assignment (lvalue, expr) ->
    compile_assignment lvalue expr ~symtable ~scope
  | If (expr, stmt) ->
    [`If (compile_expression_to_comparison expr ~symtable ~scope,
          compile_statement stmt ~symtable ~scope)]
  | IfElse (expr, then_stmt, else_stmt) ->
    [`IfElse (compile_expression_to_comparison expr ~symtable ~scope,
              compile_statement then_stmt ~symtable ~scope,
              compile_statement else_stmt ~symtable ~scope)]
  | While _
  | Global _
  | Empty ->
    []

and compile_assignment
    (lvalue : Batsh_ast.leftvalue)
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  match expr with
  | String _
  | StrCompare _
  | Concat _
  | Call _
  | Leftvalue _ ->
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    [`Assignment (lvalue, compile_expression expr ~symtable ~scope)]
  | Bool _
  | Int _
  | Float _
  | ArithUnary _
  | ArithBinary _ ->
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    [`ArithAssign (lvalue, compile_expression_to_arith expr ~symtable ~scope)]
  | List exprs ->
    List.concat (List.mapi exprs ~f: (fun i expr ->
        compile_assignment (ListAccess (lvalue, (Int i))) expr ~symtable ~scope
      ))

and compile_statements
    (stmts: Batsh_ast.statements)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :statements =
  List.fold stmts ~init: [] ~f: (fun acc stmt ->
      let stmts = compile_statement stmt ~symtable ~scope in
      acc @ stmts
    )

let compile_function
    (name, params, stmts)
    ~(symtable : Symbol_table.t)
  : statements =
  (* let scope = Symbol_table.scope symtable name in *)
  (* let body = compile_statements stmts ~symtable ~scope in *)
  [] (* TODO implement function *)

let compile_toplevel
    ~(symtable : Symbol_table.t)
    (topl: Batsh_ast.toplevel)
  : statements =
  match topl with
  | Statement stmt ->
    compile_statement stmt ~symtable
      ~scope: (Symbol_table.global_scope symtable)
  | Function func ->
    compile_function func ~symtable

let compile (batsh: Batsh.t) : t =
  let ast = Batsh.ast batsh in
  let symtable = Batsh.symtable batsh in
  let transformed_ast = Winbat_transform.split ast ~symtable in
  let stmts = List.fold transformed_ast ~init: [] ~f: (fun acc topl ->
      let stmts = compile_toplevel topl ~symtable in
      acc @ stmts
    ) in
  (`Raw "@echo off")
  :: (`Raw "setlocal EnableDelayedExpansion")
  :: (`Raw "setlocal EnableExtensions")
  :: stmts
