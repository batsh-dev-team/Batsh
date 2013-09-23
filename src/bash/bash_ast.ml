type identifier = string

type identifiers = identifier list

type leftvalue =
  | Identifier of identifier
  | ListAccess of (leftvalue * arithmetic)

and arithmetic =
  | Leftvalue of leftvalue
  | Int of int
  | Float of float
  | ArithUnary of (string * arithmetic)
  | ArithBinary of (string * arithmetic * arithmetic)
  | Parentheses of arithmetic
  | ArithTemp of (identifier * expression)

and expression =
  | Variable of leftvalue
  | String of string
  | Result of arithmetic
  | StrBinary of (string * expression * expression)
  | Command of (expression * expressions)
  | List of expressions
  | StrTemp of (identifier * expression)

and expressions = expression list

type statement =
  | Comment of string
  | Local of identifier
  | Assignment of (leftvalue * expression)
  | Expression of expression
  | If of (expression * statement)
  | IfElse of (expression * statement * statement)
  | While of (expression * statement)
  | Block of statements
  | Empty

and statements = statement list

type toplevel =
  | Statement of statement
  | Function of (identifier * statements)

type t = toplevel list
