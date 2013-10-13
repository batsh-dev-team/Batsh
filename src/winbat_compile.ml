open Core.Std
open Batsh_ast
open Winbat_ast

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

let compile_expression
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : varstrings =
  let rec compile_expression_impl
      (expr : Batsh_ast.expression)
    : varstring Dlist.t =
    match expr with
    | Bool false ->
      Dlist.of_list [`Str "0"]
    | Bool true ->
      Dlist.of_list [`Str "1"]
    | Int num ->
      Dlist.of_list [`Str (string_of_int num)]
    | String str ->
      Dlist.of_list [`Str str]
    | Leftvalue lvalue ->
      Dlist.of_list [`Var (compile_leftvalue lvalue ~symtable ~scope)]
    | Concat (left, right) ->
      let left = compile_expression_impl left in
      let right = compile_expression_impl right in
      Dlist.append left right
    | Call _ ->
      failwith "Bug: Call must have been split into assignments."
    | _ ->
      Sexp.output_hum stderr (Batsh_ast.sexp_of_expression expr);
      assert false (* TODO *)
  in
  Dlist.to_list (compile_expression_impl expr)

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

let compile_call
    (ident, exprs)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : (statements * leftvalue) =
  let exprs = compile_expressions exprs ~symtable ~scope in
  if Symbol_table.is_function symtable ident then
    (* function call *)
    let frame_pointer_assign, frame_pointer =
      if Symbol_table.Scope.is_function scope then
        (* increase frame pointer %~2 by 1 *)
        let frame_pointer = `Identifier (
            Symbol_table.Scope.add_temporary_variable scope)
        in
        [`ArithAssign (
            frame_pointer,
            `ArithBinary ("+", `Int 1, `Var (`Identifier "%~2"))
          )
        ], `Var (frame_pointer)
      else
        [], `Str "0"
    in
    let retval = Symbol_table.Scope.add_temporary_variable scope in
    (frame_pointer_assign @
       [`Call 
          (`Str ("call :" ^ ident),
           [`Str retval; (* return value *)
            frame_pointer (* frame pointer *)
           ] @ exprs
          )
       ]), `Identifier retval
  else
    (* external command *)
    (* TODO return value*)
    [`Call (`Str ident, exprs)], `Identifier "_"

let rec compile_expression_statement
    (expr : Batsh_ast.expression)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  match expr with
  | Call call ->
    let stmts, _ = compile_call call ~symtable ~scope in
    stmts
  | Leftvalue _ ->
    [] (* No side effect *)
  | _ ->
    Sexp.output_hum stderr (Batsh_ast.sexp_of_expression expr); 
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
    compile_expression_statement expr ~symtable ~scope
  | Assignment (lvalue, expr) ->
    compile_assignment lvalue expr ~symtable ~scope
  | If (expr, stmt) ->
    [`If (compile_expression_to_comparison expr ~symtable ~scope,
          compile_statement stmt ~symtable ~scope)]
  | IfElse (expr, then_stmt, else_stmt) ->
    [`IfElse (compile_expression_to_comparison expr ~symtable ~scope,
              compile_statement then_stmt ~symtable ~scope,
              compile_statement else_stmt ~symtable ~scope)]
  | While (expr, stmt) ->
    let condition = compile_expression_to_comparison expr ~symtable ~scope in
    let body = compile_statement stmt ~symtable ~scope in
    let label = sprintf "WHILE_%d" (Random.int 32768) in
    (* TODO check conflict *)
    [
      `Label label;
      `If (condition, body @ [`Goto label]);
    ]
  | Return (Some expr) ->
    [
      `Assignment (`Identifier "%~1", compile_expression expr ~symtable ~scope);
      `Goto ":EOF"
    ]
  | Return None ->
    [`Goto ":EOF"]
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
  | Concat _
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
  | Call call ->
    let stmts, retval = compile_call call ~symtable ~scope in
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    stmts @ [`Assignment (lvalue, [`Var retval])]
  | StrCompare _ ->
    let comp = compile_expression_to_comparison expr ~symtable ~scope in
    let lvalue = compile_leftvalue lvalue ~symtable ~scope in
    [`IfElse (
        comp,
        [`ArithAssign (lvalue, `Int 1)],
        [`ArithAssign (lvalue, `Int 0)]
      )
    ]

and compile_statements
    (stmts: Batsh_ast.statements)
    ~(symtable: Symbol_table.t)
    ~(scope: Symbol_table.Scope.t)
  : statements =
  Dlist.to_list (
    List.fold stmts ~init: (Dlist.empty ()) ~f: (fun acc stmt ->
        let stmts = compile_statement stmt ~symtable ~scope in
        Dlist.append acc (Dlist.of_list stmts)
      )
  )

(* Function variable, call, return replacement *)

let rec compile_function_leftvalue
    (lvalue : leftvalue)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : leftvalue =
  match lvalue with
  | `Identifier ident ->
    if Symbol_table.Scope.is_global_variable scope ~name: ident then
      lvalue
    else
      (* Add surfix _%~2 to local variable *)
      `ListAccess (lvalue, `Var (`Identifier "%~2"))
  | `ListAccess (lvalue, index) ->
    `ListAccess (compile_function_leftvalue lvalue ~symtable ~scope, index)

let compile_function_varstring
    (var : varstring)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : varstring =
  match var with
  | `Var lvalue ->
    `Var (compile_function_leftvalue lvalue ~symtable ~scope)
  | `Str _ ->
    var

let compile_function_varstrings
    (vars : varstrings)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : varstrings =
  List.map vars ~f: (compile_function_varstring ~symtable ~scope)

let rec compile_function_arithmetic
    (arith : arithmetic)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : arithmetic =
  match arith with
  | `Var lvalue ->
    `Var (compile_function_leftvalue lvalue ~symtable ~scope)
  | `Int _ ->
    arith
  | `ArithUnary (operator, arith) ->
    `ArithUnary (operator, compile_function_arithmetic arith ~symtable ~scope)
  | `ArithBinary (operator, left, right) ->
    `ArithBinary (operator,
                  compile_function_arithmetic left ~symtable ~scope,
                  compile_function_arithmetic right ~symtable ~scope)

let compile_function_comparison
    (cond : comparison)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : comparison =
  match cond with
  | `StrCompare (operator, left, right) ->
    `StrCompare (operator,
                 compile_function_varstrings left ~symtable ~scope,
                 compile_function_varstrings right ~symtable ~scope)

let rec compile_function_statement
    (stmt : statement)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statement =
  match stmt with
  | `Comment _ | `Raw _ | `Label _ | `Goto _ | `Empty ->
    stmt
  | `Assignment (lvalue, vars) ->
    `Assignment (compile_function_leftvalue lvalue ~symtable ~scope,
                 compile_function_varstrings vars ~symtable ~scope)
  | `ArithAssign (lvalue, arith) ->
    `ArithAssign (compile_function_leftvalue lvalue ~symtable ~scope,
                  compile_function_arithmetic arith ~symtable ~scope)
  | `Call (name, params) ->
    `Call (compile_function_varstring name ~symtable ~scope,
           compile_function_varstrings params ~symtable ~scope)
  | `If (cond, stmts) ->
    `If (compile_function_comparison cond ~symtable ~scope,
         compile_function_statements stmts ~symtable ~scope)
  | `IfElse (cond, then_stmts, else_stmts) ->
    `IfElse (compile_function_comparison cond ~symtable ~scope,
             compile_function_statements then_stmts ~symtable ~scope,
             compile_function_statements else_stmts ~symtable ~scope)

and compile_function_statements
    (stmts : statements)
    ~(symtable : Symbol_table.t)
    ~(scope : Symbol_table.Scope.t)
  : statements =
  List.map stmts ~f: (compile_function_statement ~symtable ~scope)

let compile_function
    (name, params, stmts)
    ~(symtable : Symbol_table.t)
  : statements =
  let scope = Symbol_table.scope symtable name in
  let body = compile_statements stmts ~symtable ~scope in
  let replaced_body = compile_function_statements body ~symtable ~scope in
  let params_assignments = List.mapi params ~f: (fun i param ->
      (* Add frame pointer surfix to every paramemeter *)
      let lvalue = `ListAccess (`Identifier param, `Var (`Identifier "%~2")) in
      let param_var = `Identifier (sprintf "%%~%d" (i + 3)) in
      `Assignment (lvalue, [`Var param_var])
    )
  in
  (`Empty
   :: (`Goto ":EOF")
   :: (`Label name)
   :: params_assignments)
  @ replaced_body

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

let sort_functions (topls : Batsh_ast.t) : Batsh_ast.t =
  let is_function topl : bool =
    match topl with
    | Function _ -> true
    | Statement _ -> false
  in
  List.stable_sort topls ~cmp: (fun a b ->
      let func_a = is_function a in
      let func_b = is_function b in
      match (func_a, func_b) with
      | (true, true) -> 0
      | (true, false) -> 1
      | (false, true) -> -1
      | (false, false) -> 0
    )

let compile (batsh: Parser.t) : t =
  let ast = Parser.ast batsh in
  let symtable = Parser.symtable batsh in
  let transformed_ast = Winbat_transform.split ast ~symtable in
  let sorted_ast = sort_functions transformed_ast in
  let stmts = Dlist.to_list (
      List.fold_left sorted_ast
        ~init: (Dlist.empty ())
        ~f: (fun acc topl ->
            let stmts = compile_toplevel topl ~symtable in
            Dlist.append acc (Dlist.of_list stmts)
          )
    )
  in
  (`Raw "@echo off")
  :: (`Raw "setlocal EnableDelayedExpansion")
  :: (`Raw "setlocal EnableExtensions")
  :: (`Empty)
  :: stmts
