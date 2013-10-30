open Core_kernel.Std

type identifier = string

and identifiers = identifier list

and leftvalue =
  | Identifier of identifier
  | ListAccess of (leftvalue * expression)

and expression =
  | Bool of bool
  | Float of float
  | Int of int
  | List of expressions
  | String of string
  | Leftvalue of leftvalue
  | ArithUnary of (string * expression)
  | ArithBinary of (string * expression * expression)
  | Concat of (expression * expression)
  | StrCompare of (string * expression * expression)
  | Call of (identifier * expressions)

and expressions = expression list

and statement =
  | Comment of string
  | Block of statements
  | Expression of expression
  | Assignment of (leftvalue * expression)
  | If of (expression * statement)
  | IfElse of (expression * statement * statement)
  | While of (expression * statement)
  | Global of identifier
  | Return of expression option
  | Empty

and statements = statement list

and toplevel =
  | Statement of statement
  | Function of (identifier * identifiers * statements)

and t = toplevel list
with sexp_of
