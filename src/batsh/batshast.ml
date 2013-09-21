type identifier = string

type identifiers = identifier list

type leftvalue =
  | Identifier of identifier
  | ListAccess of (leftvalue * expression)

and expression =
  | Bool of bool
  | Float of float
  | Int of int
  | List of expression list
  | String of string
  | Leftvalue of leftvalue
  | ArithBinary of (string * expression * expression)
  | StrBinary of (string * expression * expression)
  | Parentheses of expression
  | Call of (identifier * expression list)

type statement =
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

type toplevel =
  | Statement of statement
  | Function of (identifier * identifiers * statements)

type asttype = toplevel list
