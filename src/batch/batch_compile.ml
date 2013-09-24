open Core.Std
open Batch_ast

module BAST = Batsh_ast
module Symbol_table = Batsh.Symbol_table

let rec compile_leftvalue
    (lvalue: BAST.leftvalue)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  : identifier =
  match lvalue with
  | BAST.Identifier ident ->
    ident

let rec compile_expr
    (expr: BAST.expression)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :expression =
  let compile_expr = compile_expr ~symtable ~scope in
  match expr with
  | BAST.Parentheses expr ->
    compile_expr expr

let rec compile_statement
    (stmt : BAST.statement)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statement =
  match stmt with
  | BAST.Comment comment ->
    Comment comment
  | BAST.Assignment (lvalue, expr) ->
    Assignment (compile_leftvalue lvalue ~symtable ~scope,
                compile_expr expr ~symtable ~scope)
  | BAST.Block stmts ->
    Block (List.map stmts ~f: (compile_statement ~symtable ~scope))
  | BAST.Global _ ->
    Empty
  | BAST.Empty ->
    Empty

let compile_statements
    (stmts: BAST.statements)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  :statements =
  List.map stmts ~f: (compile_statement ~symtable ~scope)

let compile_function
    (name, params, stmts)
    ~(symtable : Symbol_table.t)
  : statement =
  let scope = Symbol_table.scope symtable name in
  let body = compile_statements stmts ~symtable ~scope in
  Block body

let compile_toplevel
    ~(symtable : Symbol_table.t)
    (topl: BAST.toplevel)
  : statement =
  match topl with
  | BAST.Statement stmt ->
    compile_statement stmt ~symtable
      ~scope: (Symbol_table.global_scope symtable)
  | BAST.Function func ->
    compile_function func ~symtable

let compile (batsh: Batsh.t) : t =
  let program = Batsh.ast batsh in
  let symtable = Batsh.symtable batsh in
  List.map program ~f: (compile_toplevel ~symtable)
