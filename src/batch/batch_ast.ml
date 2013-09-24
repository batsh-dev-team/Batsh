type identifier = string

type identifiers = identifier list

type expression =
  | Identifier of identifier

type statement =
  | Comment of string
  | Assignment of (identifier * expression)
  | Block of statements
  | Empty

and statements = statement list

type t = statements
