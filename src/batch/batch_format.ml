open Core.Std
open Batch_ast

let rec print_expression outx (expr: expression) =
  match expr with
  | Identifier ident ->
    fprintf outx "%s" ident

let rec print_statement out (stmt: statement) ~(indent: int) =
  let () = match stmt with
    | Block _ -> ()
    | _ ->
      Formatutil.print_indent out indent in
  match stmt with
  | Comment comment ->
    fprintf out "#%s" comment
  | Assignment (ident, expr) ->
    fprintf out "set %s=%a"
      ident
      print_expression expr
  | Block stmts ->
    print_statements out stmts ~indent
  | Empty -> ()

and print_statements: out_channel -> statements -> indent:int -> unit =
  Formatutil.print_statements ~f: print_statement

let print (outx: out_channel) (program: t) :unit =
  print_statements outx program ~indent: 0
