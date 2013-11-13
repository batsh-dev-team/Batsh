open Core_kernel.Std
open Batsh_ast

exception Error of string

let check_function_statement (stmt : statement) =
  match stmt with
  | Return (Some List _) ->
    raise (Error "list can not be used as a return value")
  | _ ->
    ()

let check_function (func : (identifier * identifiers * statements)) =
  let name, params, stmts = func in
  List.iter stmts ~f: check_function_statement

let check_toplevel (topl : toplevel) =
  match topl with
  | Statement (Global _) ->
    raise (Error "qualifier 'global' must be used in a function")
  | Statement (Return _) ->
    raise (Error "statement 'return' must be used in a function")
  | Statement _ ->
    ()
  | Function func ->
    check_function func

let check (ast : t) : unit =
  List.iter ast ~f: check_toplevel
