open Core.Std
open Batshast

type variable_entry = string

type variable_table = (string, variable_entry) Hashtbl.t

type function_entry =
  | Declare
  | Define of variable_table

type t = {
  functions : (string, function_entry) Hashtbl.t;
  globals : variable_table;
}

let rec process_leftvalue
    (scope: variable_table)
    (lvalue: leftvalue) =
  match lvalue with
  | Identifier ident ->
    Hashtbl.change scope ident (fun _ -> Some ident)
  | ListAccess (lvalue, _) ->
    process_leftvalue scope lvalue

let process_statement
    (scope: variable_table)
    (stmt: statement) =
  match stmt with
  | Assignment (lvalue, _) -> process_leftvalue scope lvalue
  | _ -> ()

let process_toplevel (symtable: t) (topl: toplevel) =
  match topl with
  | Statement stmt -> process_statement symtable.globals stmt
  | Function (name, _, stmts) ->
    match Hashtbl.find symtable.functions name with
    | Some _ -> () (* TODO duplicate *)
    | None ->
      let variables = Hashtbl.create ~hashable: String.hashable () in
      Hashtbl.change symtable.functions name (fun original ->
          (* TODO declaration *)
          Some (Define variables)
        );
      List.iter stmts ~f: (process_statement variables)

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
