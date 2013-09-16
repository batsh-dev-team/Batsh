open Core.Std
open Batshast

type symtable_entry =
  | Variable of variable_entry
  | Function of function_entry

and variable_entry = {
  name : string;
  scope : function_entry option;
}

and function_entry = string

type t = (string, symtable_entry) Hashtbl.t

let rec process_leftvalue
    (symtable: t)
    (scope: function_entry option)
    (lvalue: leftvalue) =
  match lvalue with
  | Identifier ident ->
    (* TODO local and global *)
    Hashtbl.change symtable ident (fun original ->
        Some (Variable {
            name = ident;
            scope;
          })
      );
  | ListAccess (lvalue, _) -> process_leftvalue symtable scope lvalue

let process_statement
    (symtable: t)
    (scope: function_entry option)
    (stmt: statement) =
  match stmt with
  | Assignment (lvalue, _) -> process_leftvalue symtable scope lvalue
  | _ -> ()

let process_toplevel (symtable: t) (topl: toplevel) =
  match topl with
  | Statement stmt -> process_statement symtable None stmt
  | Function (name, _, stmts) ->
    Hashtbl.change symtable name (fun original ->
        (* TODO duplicate *)
        Some (Function name)
      );
    List.iter stmts ~f: (process_statement symtable (Some name))

let create (ast: asttype) :t =
  let symtable = Hashtbl.create ~hashable: String.hashable () in
  List.iter ast ~f: (process_toplevel symtable);
  symtable

let find_function (symtable: t) (name: string) :bool =
  match Hashtbl.find symtable name with
  | Some _ -> true
  | None -> false
