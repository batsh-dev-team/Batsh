open Core.Std
open Winbat_ast

let escape (str : string) : string =
  let buffer = Buffer.create (String.length str) in
  String.iter str ~f:(fun ch ->
      let escaped = match ch with
        | '%' -> "%%"
        | '^' -> "^^"
        | '&' -> "^&"
        | '<' -> "^<"
        | '>' -> "^>"
        | '\'' -> "^'"
        | '"' -> "^\""
        | '`' -> "^`"
        | ',' -> "^,"
        | ';' -> "^;"
        | '=' -> "^="
        | '(' -> "^("
        | ')' -> "^)"
        | '!' -> "^!"
        | '\n' -> "^\n"
        | _ -> String.of_char ch
      in
      Buffer.add_string buffer escaped
    );
  Buffer.contents buffer

let rec print_leftvalue
    (buf : Buffer.t)
    (lvalue : leftvalue)
    ~(bare : bool)
  =
  match lvalue with
  | `Identifier ident ->
    if bare || ((String.get ident 0) = '%') then
      bprintf buf "%s" ident
    else
      bprintf buf "!%s!" ident
  | `ListAccess (lvalue, index) ->
    if bare then
      bprintf buf "%a_%a"
        (print_leftvalue ~bare: true) lvalue
        (print_varint ~bare: true) index
    else
      bprintf buf "!%a_%a!" 
        (print_leftvalue ~bare: true) lvalue
        (print_varint ~bare: true) index

and print_varint
    (buf : Buffer.t)
    (index : varint)
    ~(bare : bool)
  =
  match index with
  | `Var lvalue ->
    (print_leftvalue ~bare) buf lvalue
  | `Int num ->
    bprintf buf "%d" num

let rec print_arith buf (arith : arithmetic) =
  match arith with
  | `Var lvalue ->
    print_leftvalue buf lvalue ~bare: false
  | `Int num ->
    bprintf buf "%d" num
  | `ArithUnary (operator, arith) ->
    bprintf buf "%s^(%a^)" operator print_arith arith
  | `ArithBinary (operator, left, right) -> (
      let operator = if operator = "%" then "%%" else operator in
      bprintf buf "^(%a %s %a^)"
        print_arith left
        operator
        print_arith right
    )

let print_varstring buf (var : varstring) =
  match var with
  | `Var lvalue ->
    print_leftvalue buf lvalue ~bare: false
  | `Str str ->
    bprintf buf "%s" (escape str)
  | `Cont -> ()

let print_varstrings buf (vars : varstrings) ~(separater : string) =
  let comsume = ref false in
  List.iter vars ~f: (fun var ->
      match var with
      | `Cont -> comsume := true
      | _ ->
        if !comsume then
          comsume := false
        else
          Buffer.add_string buf separater;
        print_varstring buf var
    )

let print_comparison buf (condition : comparison) =
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
      bprintf buf "%a %s %a"
        (print_varstrings ~separater: "") left
        sign
        (print_varstrings ~separater: "") right
    )

let rec print_statement buf (stmt: statement) ~(indent: int) =
  Formatutil.print_indent buf indent;
  match stmt with
  | `Comment comment ->
    bprintf buf "::%s" comment
  | `Raw str ->
    Buffer.add_string buf str
  | `Label lbl ->
    bprintf buf ":%s" lbl
  | `Goto lbl ->
    bprintf buf "goto %s" lbl
  | `Assignment (lvalue, vars) ->
    bprintf buf "set %a=%a"
      (print_leftvalue ~bare: true) lvalue
      (print_varstrings ~separater: "") vars
  | `ArithAssign (lvalue, arith) ->
    bprintf buf "set /a %a=%a"
      (print_leftvalue ~bare: true) lvalue
      print_arith arith
  | `Call (name, params) ->
    bprintf buf "%a%a"
      print_varstring name
      (print_varstrings ~separater: " ") params
  | `If (condition, stmts) ->
    bprintf buf "if /i %a (\n%a\n%a)"
      print_comparison condition
      (print_statements ~indent: (indent + 2)) stmts
      Formatutil.print_indent indent
  | `IfElse (condition, then_stmts, else_stmts) ->
    bprintf buf "if /i %a (\n%a\n%a) else (\n%a\n%a)"
      print_comparison condition
      (print_statements ~indent: (indent + 2)) then_stmts
      Formatutil.print_indent indent
      (print_statements ~indent: (indent + 2)) else_stmts
      Formatutil.print_indent indent
  | `Empty -> ()

and print_statements: Buffer.t -> statements -> indent:int -> unit =
  Formatutil.print_statements ~f: print_statement

let print (buf: Buffer.t) (program: t) :unit =
  print_statements buf program ~indent: 0
