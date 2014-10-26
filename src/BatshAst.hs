module BatshAst where

data Literal = Bool Bool
  | Int Int
  | Float Float
  | String String
  | List [Expression]
  deriving (Eq,Show)

type Identifier = String

data UnaryOperator = Not | Negative
  deriving (Eq,Show)

data BinaryOperator = Equal | Plus | Minus | Multiply | Divide
  | Modulo | Concat
  deriving (Eq,Show)

data LeftValue = Identifier Identifier
  | ListAccess (LeftValue, Expression)
  deriving (Eq,Show)

data Expression = LeftValue LeftValue
  | Literal Literal
  | Unary (UnaryOperator, Expression)
  | Binary (BinaryOperator, Expression, Expression)
  | Assign (LeftValue, Expression)
  | Call (Identifier, [Parameter])
  deriving (Eq,Show)

type Parameter = Expression

data Statement = Comment String
  | Block [Statement]
  | Expression Expression
  | If (Expression, Statement)
  | IfElse (Expression, Statement, Statement)
  | While (Expression, Statement)
  | Global Identifier
  | Return (Maybe Expression)
  deriving (Eq,Show)

data TopLevel = Statement Statement
  | Function (Identifier, [Identifier], [Statement])
  deriving (Eq,Show)

type Program = [TopLevel]
