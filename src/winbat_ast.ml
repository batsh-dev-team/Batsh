type identifier = string

type identifiers = identifier list

type label = string

type varint = [
  | `Var of leftvalue
  | `Int of int
]

and leftvalue = [
  | `Identifier of identifier
  | `ListAccess of (leftvalue * varint)
]

and arithmetic = [
  | `Var of leftvalue
  | `Int of int
  | `ArithUnary of (string * arithmetic)
  | `ArithBinary of (string * arithmetic * arithmetic)
]

type varstring = [
  | `Var of leftvalue
  | `Str of string
]

and varstrings = varstring list

type comparison = [
  | `StrCompare of (string * varstrings * varstrings)
]

type statement = [
  | `Comment of string
  | `Raw of string
  | `Label of label
  | `Goto of label
  | `Assignment of (leftvalue * varstrings)
  | `ArithAssign of (leftvalue * arithmetic)
  | `Call of (varstring * varstrings)
  | `If of (comparison * statements)
  | `IfElse of (comparison * statements * statements)
  | `Empty
]

and statements = statement list

type t = statements
