type identifier = string

type identifiers = identifier list

type label = string

type varint =
  | Var of leftvalue
  | Integer of int

and leftvalue =
  | Identifier of identifier
  | ListAccess of (leftvalue * varint)

and arithmetic =
  | Leftvalue of leftvalue
  | Int of int
  | ArithUnary of (string * arithmetic)
  | ArithBinary of (string * arithmetic * arithmetic)

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
  | Call of (varstring * varstrings)
  | Empty

and statements = statement list

type t = statements
