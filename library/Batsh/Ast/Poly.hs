module Batsh.Ast.Poly where

data PLiteral a
  = Bool Bool a
  | Int Int a
  | Float Float a
  | String String a
  | List [PExpression a] a
  deriving (Eq, Read, Show)

data PLeftValue a
  = Identifier Identifier a
  | ListAccess (PLeftValue a, PExpression a) a
  deriving (Eq, Read, Show)

data PUnaryOperator a
  = Not a
  | Negate a
  deriving (Eq, Read, Show)

data PBinaryOperator a
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

data PExpression a
  = LeftValue (PLeftValue a) a
  | Literal (PLiteral a) a
  | Unary (PUnaryOperator a, PExpression a) a
  | Binary (PBinaryOperator a, PExpression a, PExpression a) a
  | Assign (PLeftValue a, PExpression a) a
  | Call (FunctionName, [PExpression a]) a
  deriving (Eq, Read, Show)

data PStatement a
  = Comment String a
  | Block [PStatement a] a
  | Expression (PExpression a) a
  | If (PExpression a, PStatement a) a
  | IfElse (PExpression a, PStatement a, PStatement a) a
  | While (PExpression a, PStatement a) a
  | Global Identifier a
  | Return (Maybe (PExpression a)) a
  deriving (Eq, Read, Show)

data PTopLevel a
  = Statement (PStatement a) a
  | Function (FunctionName, [Parameter], [PStatement a]) a
  deriving (Eq, Read, Show)

data PProgram a = Program [PTopLevel a] a deriving (Eq, Read, Show)

type Identifier = String

type FunctionName = Identifier

type Parameter = Identifier

class AstNode a where
  annot :: a annot -> annot

instance AstNode PLiteral where
  annot node = case node of
    Bool _ annot -> annot
    Int _ annot -> annot
    Float _ annot -> annot
    String _ annot -> annot
    List _ annot -> annot

instance AstNode PLeftValue where
  annot node = case node of
    Identifier _ annot -> annot
    ListAccess _ annot -> annot

instance AstNode PUnaryOperator where
  annot node = case node of
    Not annot -> annot
    Negate annot -> annot

instance AstNode PBinaryOperator where
  annot node = case node of
    Plus annot -> annot
    Minus annot -> annot
    Multiply annot -> annot
    Divide annot -> annot
    Modulo annot -> annot
    Concat annot -> annot
    Equal annot -> annot
    NotEqual annot -> annot
    ArithEqual annot -> annot
    ArithNotEqual annot -> annot
    Greater annot -> annot
    Less annot -> annot
    GreaterEqual annot -> annot
    LessEqual annot -> annot
    And annot -> annot
    Or annot -> annot

instance AstNode PExpression where
  annot node = case node of
    LeftValue _ annot -> annot
    Literal _ annot -> annot
    Unary _ annot -> annot
    Binary _ annot -> annot
    Assign _ annot -> annot
    Call _ annot -> annot

instance AstNode PStatement where
  annot node = case node of
    Comment _ annot -> annot
    Block _ annot -> annot
    Expression _ annot -> annot
    If _ annot -> annot
    IfElse _ annot -> annot
    While _ annot -> annot
    Global _ annot -> annot
    Return _ annot -> annot

instance AstNode PTopLevel where
  annot node = case node of
    Statement _ annot -> annot
    Function _ annot -> annot

instance AstNode PProgram where
  annot node = case node of
    Program _ annot -> annot

class Operator a where
  precedence :: a -> Int
  operatorStr :: a -> String

instance Operator (PUnaryOperator a) where
  precedence operator = case operator of
    Negate _ -> 7
    Not _ -> 7

  operatorStr operator = case operator of
    Not _ -> "!"
    Negate _ -> "-"

instance Operator (PBinaryOperator a) where
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
