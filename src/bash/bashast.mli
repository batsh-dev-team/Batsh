type identifier = string

type identifiers = identifier list

type leftvalue =
  | Identifier of identifier
  | ListAccess of (leftvalue * arithmetic)

and arithmetic =
  | Leftvalue of leftvalue
  | Int of int
  | Float of float
  | ArithBinary of (string * arithmetic * arithmetic)
  | Parentheses of arithmetic
  | Temporary of (identifier * expression)

and expression =
  | Variable of leftvalue
  | String of string
  | Result of arithmetic
  | Concat of (expression * expression)
  | Command of (identifier * expressions)
  | List of expressions
  | SEQ of (expression * expression)
  | SNE of (expression * expression)
  | SGT of (expression * expression)
  | SLT of (expression * expression)

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

type asttype = toplevel list
