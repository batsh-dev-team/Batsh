open Core_kernel.Std

type identifier = string

and identifiers = identifier list

and label = string

and varint = [
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

and varstring = [
  | `Var of leftvalue
  | `Str of string
  | `Rawstr of string
]

and varstrings = varstring list

and comparison = [
  | `UniCompare of (string * varstrings)
  | `StrCompare of (string * varstrings * varstrings)
  | `TestCompare of (string * varstrings)
]

and parameter = varstrings

and parameters = parameter list

and statement = [
  | `Comment of string
  | `Raw of string
  | `Label of label
  | `Goto of label
  | `Assignment of (leftvalue * varstrings)
  | `ArithAssign of (leftvalue * arithmetic)
  | `Call of (varstrings * parameters)
  | `Output of (leftvalue * varstrings * parameters)
  | `If of (comparison * statements)
  | `IfElse of (comparison * statements * statements)
  | `Empty
]

and statements = statement list

and t = statements
with sexp_of
