type identifier = string

type arithmetic =
  | Identifier of identifier
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
  | Variable of identifier
  | String of string
  | Result of arithmetic
  | Concat of (expression * expression)
  | SEQ of (expression * expression)
  | SNE of (expression * expression)
  | SGT of (expression * expression)
  | SLT of (expression * expression)
  | Command of (expression * expressions)

and expressions = expression list

type statement = 
  | Let of (identifier * arithmetic)
  | Assignment of (identifier * expression)
  | Expression of expression
  | If of (expression * statements)
  | IfElse of (expression * statements * statements)
  | While of (expression * statements)
  | Empty

and statements = statement list
