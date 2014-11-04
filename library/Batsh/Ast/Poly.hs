module Batsh.Ast.Poly where

data Literal a
  = Bool Bool a
  | Int Int a
  | Float Float a
  | String String a
  | List [Expression a] a
  deriving (Eq, Read, Show)

data LeftValue a
  = Identifier Identifier a
  | ListAccess (LeftValue a, Expression a) a
  deriving (Eq, Read, Show)

data UnaryOperator a
  = Not a
  | Negate a
  deriving (Eq, Read, Show)

data BinaryOperator a
  = Plus a
  | Minus a
  | Multiply a
  | Divide a
  | Modulo a
  | Concat a
  | Equal a
  | NotEqual a
  | ArithEqual a
  | ArithNotEqual a
  | Greater a
  | Less a
  | GreaterEqual a
  | LessEqual a
  | And a
  | Or a
  deriving (Eq, Read, Show)

data Expression a
  = LeftValue (LeftValue a) a
  | Literal (Literal a) a
  | Unary (UnaryOperator a, Expression a) a
  | Binary (BinaryOperator a, Expression a, Expression a) a
  | Assign (LeftValue a, Expression a) a
  | Call (FunctionName, [Expression a]) a
  deriving (Eq, Read, Show)

data Statement a
  = Comment String a
  | Block [Statement a] a
  | Expression (Expression a) a
  | If (Expression a, Statement a) a
  | IfElse (Expression a, Statement a, Statement a) a
  | While (Expression a, Statement a) a
  | Global Identifier a
  | Return (Maybe (Expression a)) a
  deriving (Eq, Read, Show)

data TopLevel a
  = Statement (Statement a) a
  | Function (FunctionName, [Parameter], [Statement a]) a
  deriving (Eq, Read, Show)

data Program a = Program [TopLevel a] a deriving (Eq, Read, Show)

type Identifier = String

type FunctionName = Identifier

type Parameter = Identifier

class Operator a where
  precedence :: a -> Int
  operatorStr :: a -> String

instance Operator (UnaryOperator a) where
  precedence operator = case operator of
    Negate _ -> 7
    Not _ -> 7

  operatorStr operator = case operator of
    Not _ -> "!"
    Negate _ -> "-"

instance Operator (BinaryOperator a) where
  precedence operator = case operator of
    Or _ -> 0
    And _ -> 1
    Equal _ -> 2
    NotEqual _ -> 2
    ArithEqual _ -> 2
    ArithNotEqual _ -> 2
    Greater _ -> 3
    Less _ -> 3
    GreaterEqual _ -> 3
    LessEqual _ -> 3
    Concat _ -> 4
    Plus _ -> 5
    Minus _ -> 5
    Multiply _ -> 6
    Divide _ -> 6
    Modulo _ -> 6

  operatorStr operator = case operator of
    Plus _ -> "+"
    Minus _ -> "-"
    Multiply _ -> "*"
    Divide _ -> "/"
    Modulo _ -> "%"
    Concat _ -> "++"
    Equal _ -> "=="
    NotEqual _ -> "!="
    ArithEqual _ -> "==="
    ArithNotEqual _ -> "!=="
    Greater _ -> ">"
    Less _ -> "<"
    GreaterEqual _ -> ">="
    LessEqual _ -> "<="
    And _ -> "&&"
    Or _ -> "||"
