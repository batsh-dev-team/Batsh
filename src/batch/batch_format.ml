open Core.Std
open Batch_ast

let rec print_leftvalue outx (lvalue : leftvalue) =
  match lvalue with
  | Identifier ident ->
    fprintf outx "%s" ident

let rec print_arith outx (arith : arithmetic) =
  match arith with
  | Leftvalue lvalue ->
    print_leftvalue outx lvalue
  | Int num ->
    fprintf outx "%d" num
  | ArithUnary (operator, arith) ->
    fprintf outx "%s%a" operator print_arith arith
  | ArithBinary (operator, left, right) ->
    fprintf outx "%a %s %a" print_arith left operator print_arith right
  | Parentheses expr ->
    fprintf outx "(%a)" print_arith expr

let print_varstring outx (var : varstring) =
  match var with
  | Variable lvalue ->
    print_leftvalue outx lvalue
  | String str ->
    output_string outx str

let print_varstrings outx (vars : varstrings) =
  List.iter vars ~f: (print_varstring outx)

let rec print_statement outx (stmt: statement) ~(indent: int) =
  match stmt with
  | Comment comment ->
    fprintf outx "::%s" comment
  | Raw str ->
    output_string outx str
  | Label lbl ->
    fprintf outx "%s:" lbl
  | Goto lbl ->
    fprintf outx "goto %s" lbl
  | Assignment (lvalue, vars) ->
    fprintf outx "set %a=%a"
      print_leftvalue lvalue
      print_varstrings vars
  | ArithAssign (lvalue, arith) ->
    fprintf outx "set /a %a=%a"
      print_leftvalue lvalue
      print_arith arith
  | Empty -> ()

and print_statements: out_channel -> statements -> indent:int -> unit =
  Formatutil.print_statements ~f: print_statement

let print (outx: out_channel) (program: t) :unit =
  print_statements outx program ~indent: 0
