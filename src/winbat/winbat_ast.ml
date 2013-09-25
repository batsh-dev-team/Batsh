type identifier = string

type identifiers = identifier list

type label = string

type leftvalue =
  | Identifier of identifier

type arithmetic =
  | Leftvalue of leftvalue
  | Int of int
  | ArithUnary of (string * arithmetic)
  | ArithBinary of (string * arithmetic * arithmetic)
  | Parentheses of arithmetic

type varstring =
  | Variable of leftvalue
  | String of string

and varstrings = varstring list

type statement =
  | Comment of string
  | Raw of string
  | Label of label
  | Goto of label
  | Assignment of (leftvalue * varstrings)
  | ArithAssign of (leftvalue * arithmetic)
  | Empty

and statements = statement list

type t = statements
