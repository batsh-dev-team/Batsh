open Core.Std

type identifier = string

and identifiers = identifier list

and leftvalue =
  | Identifier of identifier
  | ListAccess of (leftvalue * expression)

and expression =
  | Bool of bool
  | Float of float
  | Int of int
  | List of expression list
  | String of string
  | Leftvalue of leftvalue
  | ArithUnary of (string * expression)
  | ArithBinary of (string * expression * expression)
  | StrBinary of (string * expression * expression)
  | Parentheses of expression
  | Call of (identifier * expression list)

and statement =
  | Comment of string
  | Block of statements
  | Expression of expression
  | Assignment of (leftvalue * expression)
  | If of (expression * statement)
  | IfElse of (expression * statement * statement)
  | While of (expression * statement)
  | Global of identifier
  | Empty

and statements = statement list

and toplevel =
  | Statement of statement
  | Function of (identifier * identifiers * statements)

and t = toplevel list
with sexp_of
