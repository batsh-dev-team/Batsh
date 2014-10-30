module Batsh.Ast where

class Operator a where
  precedence :: a -> Int

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

instance Operator BinaryOperator where
  precedence operator = case operator of
    Or -> 0
    And -> 1
    Equal -> 2
    NotEqual -> 2
    ArithEqual -> 2
    ArithNotEqual -> 2
    Greater -> 3
    Less -> 3
    GreaterEqual -> 3
    LessEqual -> 3
    Concat -> 4
    Plus -> 5
    Minus -> 5
    Multiply -> 6
    Divide -> 6
    Modulo -> 6

instance Operator UnaryOperator where
  precedence operator = case operator of
    Negate -> 7
    Not -> 7

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
