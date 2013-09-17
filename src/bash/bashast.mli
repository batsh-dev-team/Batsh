type identifier = string

type identifiers = identifier list

type leftvalue =
  | Identifier of identifier
  | ListAccess of (leftvalue * arithmetic)

and arithmetic =
  | Leftvalue of leftvalue
  | Int of int
  | Float of float
  | Plus of (arithmetic * arithmetic)
  | Minus of (arithmetic * arithmetic)
  | Multiply of (arithmetic * arithmetic)
  | Divide of (arithmetic * arithmetic)
  | Modulo of (arithmetic * arithmetic)
  | AEQ of (arithmetic * arithmetic)
  | ANE of (arithmetic * arithmetic)
  | AGT of (arithmetic * arithmetic)
  | ALT of (arithmetic * arithmetic)
  | AGE of (arithmetic * arithmetic)
  | ALE of (arithmetic * arithmetic)
  | Parentheses of arithmetic

type expression =
  | Variable of leftvalue
  | String of string
  | Result of arithmetic
  | Concat of (expression * expression)
  | SEQ of (expression * expression)
  | SNE of (expression * expression)
  | SGT of (expression * expression)
  | SLT of (expression * expression)
  | Command of (expression * expressions)
  | List of expressions

and expressions = expression list

type statement =
  | Comment of string
  | Local of identifier
  | Let of (leftvalue * arithmetic)
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
