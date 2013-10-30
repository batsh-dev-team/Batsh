open Core_kernel.Std
open Batsh_ast

type variable_entry = {
  name : string;
  global : bool;
}
with sexp_of

type variable_table = (string, variable_entry) Hashtbl.t

let sexp_of_variable_table (vtable : variable_table) : Sexp.t =
  Sexp.List (Hashtbl.fold vtable ~init: []
               ~f: (fun ~key ~data acc ->
                   let item = (sexp_of_variable_entry data) in
                   item :: acc
                 )
            )

type function_entry =
  | Declaration
  | Defination of variable_table
with sexp_of

type t = {
  functions : (string, function_entry) Hashtbl.t;
  globals : variable_table;
}
with sexp_of

module Scope = struct
  type t =
    | GlobalScope of variable_table
    | FunctionScope of (string * variable_table)
  with sexp_of

  let is_function (scope : t) : bool =
    match scope with
    | GlobalScope _ -> false
    | FunctionScope _ -> true

  let variables (scope : t) : variable_table =
    match scope with
    | GlobalScope variables -> variables
    | FunctionScope (_, variables) -> variables

  let find_variable
      (scope: t)
      ~(name: string)
    : variable_entry option =
    Hashtbl.find (variables scope) name

  let is_global_variable
      (scope : t)
      ~(name : string)
    : bool =
    match scope with
    | GlobalScope _ -> true
    | FunctionScope (_, variable_table) ->
      match Hashtbl.find variable_table name with
      | None -> true (* if variable is not found, consider it as external *)
      | Some variable -> variable.global

  let fold
      (scope: t)
      ~(init: 'a)
      ~(f: string -> bool -> 'a -> 'a) =
    let vtable = variables scope in
    Hashtbl.fold vtable ~init
      ~f: (fun ~key ~data acc -> f data.name data.global acc)

  let add_temporary_variable
      (scope: t)
    : identifier =
    let rec find_available_name (num : int) : string =
      let name = "_" ^ (Int.to_string num) in
      match find_variable scope ~name with
      | None ->
        (* Add to symbol table *)
        let variables = variables scope in
        Hashtbl.add_exn variables ~key: name ~data: {name; global = false};
        name
      | Some _ ->
        (* Duplicated, try again *)
        find_available_name (num + 1)
    in
    find_available_name 0
end

let process_identifier
    (scope: variable_table)
    (ident: identifier)
    ~(global: bool) =
  Hashtbl.change scope ident (fun original ->
      let entry = Some {
          name = ident;
          global = global;
        }
      in
      match original with
      | None -> entry
      | Some existing ->
        if global && not existing.global then
          entry
        else
          original
    )

let rec process_leftvalue
    (scope: variable_table)
    (lvalue: leftvalue)
    ~(global: bool) =
  match lvalue with
  | Identifier ident ->
    process_identifier scope ident ~global
  | ListAccess (lvalue, _) ->
    process_leftvalue scope lvalue ~global

let process_statement
    (scope: variable_table)
    (stmt: statement) =
  match stmt with
  | Assignment (lvalue, _) -> process_leftvalue scope lvalue ~global: false
  | Global ident -> process_identifier scope ident ~global: true
  | _ -> ()

let process_function
    functions
    (name, params, stmts) =
  match Hashtbl.find functions name with
  | Some _ -> () (* TODO duplicate *)
  | None ->
    let variables = Hashtbl.create ~hashable: String.hashable () in
    Hashtbl.change functions name (fun original ->
        (* TODO declaration *)
        Some (Defination variables)
      );
    List.iter stmts ~f: (process_statement variables);
    List.iter params ~f: (process_identifier variables ~global: false)

let process_toplevel (symtable: t) (topl: toplevel) =
  match topl with
  | Statement stmt -> process_statement symtable.globals stmt
  | Function func -> process_function symtable.functions func

let create (ast: Batsh_ast.t) :t =
  let symtable = {
    functions = Hashtbl.create ~hashable: String.hashable ();
    globals = Hashtbl.create ~hashable: String.hashable ()
  } in
  List.iter ast ~f: (process_toplevel symtable);
  symtable

let scope (symtable: t) (name: string) : Scope.t =
  let variables = match Hashtbl.find_exn symtable.functions name with
    | Declaration -> failwith "No such function"
    | Defination variables -> variables
  in
  Scope.FunctionScope (name, variables)

let global_scope (symtable: t) : Scope.t =
  Scope.GlobalScope symtable.globals

let is_function (symtable : t) (name : string) : bool =
  match Hashtbl.find symtable.functions name with
  | Some _ -> true
  | None -> false
