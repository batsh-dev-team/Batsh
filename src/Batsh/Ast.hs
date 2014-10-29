module Batsh.Ast where

data Literal = Bool Bool
  | Int Int
  | Float Float
  | String String
  | List [Expression]
  deriving (Eq,Read,Show)

type Identifier = String

data LeftValue = Identifier Identifier
  | ListAccess (LeftValue, Expression)
  deriving (Eq,Read,Show)

data UnaryOperator = Not | Negate
  deriving (Eq,Read,Show)

data BinaryOperator = Plus | Minus | Multiply | Divide | Modulo | Concat
  | Equal | NotEqual | ArithEqual | ArithNotEqual | Greater | Less
  | GreaterEqual | LessEqual | And | Or
  deriving (Eq,Read,Show)

data Expression = LeftValue LeftValue
  | Literal Literal
  | Unary (UnaryOperator, Expression)
  | Binary (BinaryOperator, Expression, Expression)
  | Assign (LeftValue, Expression)
  | Call (Identifier, [Parameter])
  deriving (Eq,Read,Show)

type Parameter = Expression

data Statement = Comment String
  | Block [Statement]
  | Expression Expression
  | If (Expression, Statement)
  | IfElse (Expression, Statement, Statement)
  | While (Expression, Statement)
  | Global Identifier
  | Return (Maybe Expression)
  deriving (Eq,Read,Show)

data TopLevel = Statement Statement
  | Function (Identifier, [Identifier], [Statement])
  deriving (Eq,Read,Show)

type Program = [TopLevel]
