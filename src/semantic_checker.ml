open Core_kernel.Std
open Batsh_ast

exception Error of string

let check_toplevel (topl: toplevel) =
  match topl with
  | Statement (Global _) ->
    raise (Error "qualifier 'global' must be used in a function")
  | Statement (Return _) ->
    raise (Error "statement 'return' must be used in a function")
  | Statement _
  | Function _ ->
    ()

let check (ast : t) : unit =
  List.iter ast ~f: check_toplevel
