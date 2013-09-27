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

type statement = [
  | `Comment of string
  | `Raw of string
  | `Label of label
  | `Goto of label
  | `Assignment of (leftvalue * varstrings)
  | `ArithAssign of (leftvalue * arithmetic)
  | `Call of (varstring * varstrings)
  | `Empty
]

and statements = statement list

type t = statements
