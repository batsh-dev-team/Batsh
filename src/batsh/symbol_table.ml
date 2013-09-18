open Core.Std
open Batshast

type variable_entry = {
  name : string;
  global : bool;
}

type variable_table = (string, variable_entry) Hashtbl.t

type function_entry =
  | Declare
  | Define of variable_table

type t = {
  functions : (string, function_entry) Hashtbl.t;
  globals : variable_table;
}

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
    func =
  let name, _, stmts = func in
  match Hashtbl.find functions name with
  | Some _ -> () (* TODO duplicate *)
  | None ->
    let variables = Hashtbl.create ~hashable: String.hashable () in
    Hashtbl.change functions name (fun original ->
        (* TODO declaration *)
        Some (Define variables)
      );
    List.iter stmts ~f: (process_statement variables)

let process_toplevel (symtable: t) (topl: toplevel) =
  match topl with
  | Statement stmt -> process_statement symtable.globals stmt
  | Function func -> process_function symtable.functions func

let create (ast: asttype) :t =
  let symtable = {
    functions = Hashtbl.create ~hashable: String.hashable ();
    globals = Hashtbl.create ~hashable: String.hashable ()
  } in
  List.iter ast ~f: (process_toplevel symtable);
  symtable

let find_function (symtable: t) (name: string) :bool =
  match Hashtbl.find symtable.functions name with
  | Some _ -> true
  | None -> false

let fold_variables
    (symtable: t)
    ~(scope: string option)
    ~(init: 'a)
    ~(f: string -> bool -> 'a -> 'a) =
  let variables : variable_table = match scope with
    | Some scope -> (
        match Hashtbl.find_exn symtable.functions scope with
        | Declare -> failwith "No such function"
        | Define variables -> variables
      )
    | None -> symtable.globals
  in
  Hashtbl.fold variables ~init
    ~f: (fun ~key ~data acc -> f data.name data.global acc)
