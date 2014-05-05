open Core_kernel.Std

type identifier = string

and identifiers = identifier list

and leftvalue =
  | Identifier of identifier
  | ListAccess of (leftvalue * arithmetic)
  | EntireList of leftvalue
  | Cardinal of leftvalue

and arithmetic =
  | Leftvalue of leftvalue
  | Int of int
  | Float of float
  | ArithUnary of (string * arithmetic)
  | ArithBinary of (string * arithmetic * arithmetic)

and expression =
  | Variable of leftvalue
  | String of string
  | Result of arithmetic
  | StrBinary of (string * expression * expression)
  | TestUnary of (string * expression)
  | Command of (expression * expressions)
  | List of expressions
  | Raw of string

and expressions = expression list

and statement =
  | Comment of string
  | Local of identifier
  | Assignment of (leftvalue * expression)
  | Expression of expression
  | If of (expression * statement)
  | IfElse of (expression * statement * statement)
  | While of (expression * statement)
  | Block of statements
  | Return
  | Empty

and statements = statement list

and toplevel =
  | Statement of statement
  | Function of (identifier * statements)

and t = toplevel list
with sexp_of
