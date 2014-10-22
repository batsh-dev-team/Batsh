module BatshAst where

data Literal = Bool Bool
  | Int Int
  | Float Float
  | String String
  | List [Expression]

type Identifier = String

data UnaryOperator = Not | Negative

data BinaryOperator = Equal | Plus | Minus | Multiply | Divide

data LeftValue = Identifier Identifier
  | ListAccess (LeftValue, Expression)

data Expression = LeftValue LeftValue
  | Literal Literal
  | Unary (UnaryOperator, Expression)
  | Binary (BinaryOperator, Expression, Expression)
  | Assign (LeftValue, Expression)
  | Call (Identifier, [Parameter])

type Parameter = Expression

data Statement = Comment String
  | Block [Statement]
  | Expression Expression
  | If (Expression, Statement)
  | IfElse (Expression, Statement, Statement)
  | While (Expression, Statement)
  | Global Identifier
  | Return (Maybe Expression)

type TopLevel = [Statement]
