open Core.Std
open Winbat_ast

let rec print_leftvalue
    (outx : out_channel)
    (lvalue : leftvalue)
    ~(bare : bool)
  =
  match lvalue with
  | `Identifier ident ->
    if bare then
      fprintf outx "%s" ident
    else
      fprintf outx "!%s!" ident
  | `ListAccess (lvalue, index) ->
    if bare then
      fprintf outx "%a_%a"
        (print_leftvalue ~bare: true) lvalue
        print_varint index
    else
      fprintf outx "!%a_%a!" 
        (print_leftvalue ~bare: true) lvalue
        print_varint index

and print_varint
    (outx : out_channel)
    (index : varint)
  =
  match index with
  | `Var lvalue ->
    print_leftvalue outx lvalue ~bare: false
  | `Int num ->
    fprintf outx "%d" num

let rec print_arith outx (arith : arithmetic) =
  match arith with
  | `Var lvalue ->
    print_leftvalue outx lvalue ~bare: false
  | `Int num ->
    fprintf outx "%d" num
  | `ArithUnary (operator, arith) ->
    fprintf outx "%s(%a)" operator print_arith arith
  | `ArithBinary (operator, left, right) -> (
      let operator = if operator = "%" then "%%" else operator in
      fprintf outx "(%a %s %a)"
        print_arith left
        operator
        print_arith right
    )

let print_varstring outx (var : varstring) =
  match var with
  | `Var lvalue ->
    print_leftvalue outx lvalue ~bare: false
  | `Str str ->
    fprintf outx "%s" str

let print_varstrings outx (vars : varstrings) ~(separater : string) =
  let num_items = List.length vars in
  List.iteri vars ~f: (fun i var ->
      print_varstring outx var;
      if i < num_items - 1 then
        output_string outx separater
    )

let print_comparison outx (condition : comparison) =
  match condition with
  | `StrCompare (operator, left, right) -> (
      let sign = match operator with
        | "==" | "===" -> "EQU"
        | "!=" | "!==" -> "NEQ"
        | ">" -> "GTR"
        | "<" -> "LSS"
        | ">=" -> "GEQ"
        | "<=" -> "LEQ"
        | _ -> failwith ("Unknown operator: " ^ operator)
      in
      fprintf outx "%a %s %a"
        (print_varstrings ~separater: "") left
        sign
        (print_varstrings ~separater: "") right
    )

let rec print_statement outx (stmt: statement) ~(indent: int) =
  Formatutil.print_indent outx indent;
  match stmt with
  | `Comment comment ->
    fprintf outx "::%s" comment
  | `Raw str ->
    output_string outx str
  | `Label lbl ->
    fprintf outx "%s:" lbl
  | `Goto lbl ->
    fprintf outx "goto %s" lbl
  | `Assignment (lvalue, vars) ->
    fprintf outx "set %a=%a"
      (print_leftvalue ~bare: true) lvalue
      (print_varstrings ~separater: "") vars
  | `ArithAssign (lvalue, arith) ->
    fprintf outx "set /a %a=%a"
      (print_leftvalue ~bare: true) lvalue
      print_arith arith
  | `Call (name, params) ->
    fprintf outx "%a %a"
      print_varstring name
      (print_varstrings ~separater: " ") params
  | `If (condition, stmts) ->
    fprintf outx "if /i %a (\n%a\n%a)"
      print_comparison condition
      (print_statements ~indent: (indent + 2)) stmts
      Formatutil.print_indent indent
  | `IfElse (condition, then_stmts, else_stmts) ->
    fprintf outx "if /i %a (\n%a\n%a) else (\n%a\n%a)"
      print_comparison condition
      (print_statements ~indent: (indent + 2)) then_stmts
      Formatutil.print_indent indent
      (print_statements ~indent: (indent + 2)) else_stmts
      Formatutil.print_indent indent
  | `Empty -> ()

and print_statements: out_channel -> statements -> indent:int -> unit =
  Formatutil.print_statements ~f: print_statement

let print (outx: out_channel) (program: t) :unit =
  print_statements outx program ~indent: 0
