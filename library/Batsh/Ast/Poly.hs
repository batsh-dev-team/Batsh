module Batsh.Ast.Poly where

data PLiteral annot_type
  = Bool {literal_bool :: Bool, literal_annot :: annot_type}
  | Int {literal_int :: Int, literal_annot :: annot_type}
  | Float {literal_float :: Float, literal_annot :: annot_type}
  | String {literal_str :: String, literal_annot :: annot_type}
  | List {literal_list :: [PExpression annot_type], literal_annot :: annot_type}
  deriving (Eq, Read, Show)

data PLeftValue annot_type
  = Identifier {
    lvalue_ident :: Identifier,
    lvalue_annot :: annot_type
  }
  | ListAccess {
    lvalue_var :: PLeftValue annot_type,
    lvalue_index :: PExpression annot_type,
    lvalue_annot :: annot_type
  }
  deriving (Eq, Read, Show)

data PUnaryOperator annot_type
  = Not {unOp_annot :: annot_type}
  | Negate {unOp_annot :: annot_type}
  deriving (Eq, Read, Show)

data PBinaryOperator annot_type
  = Plus {binOp_annot :: annot_type}
  | Minus {binOp_annot :: annot_type}
  | Multiply {binOp_annot :: annot_type}
  | Divide {binOp_annot :: annot_type}
  | Modulo {binOp_annot :: annot_type}
  | Concat {binOp_annot :: annot_type}
  | Equal {binOp_annot :: annot_type}
  | NotEqual {binOp_annot :: annot_type}
  | ArithEqual {binOp_annot :: annot_type}
  | ArithNotEqual {binOp_annot :: annot_type}
  | Greater {binOp_annot :: annot_type}
  | Less {binOp_annot :: annot_type}
  | GreaterEqual {binOp_annot :: annot_type}
  | LessEqual {binOp_annot :: annot_type}
  | And {binOp_annot :: annot_type}
  | Or {binOp_annot :: annot_type}
  deriving (Eq, Read, Show)

data PExpression annot_type
  = LeftValue {
    expr_lvalue :: PLeftValue annot_type,
    expr_annot :: annot_type
  }
  | Literal {
    expr_literal :: PLiteral annot_type,
    expr_annot :: annot_type
  }
  | Unary {
    expr_unOp :: PUnaryOperator annot_type,
    expr_subExpr :: PExpression annot_type,
    expr_annot :: annot_type
  }
  | Binary {
    expr_binOp :: PBinaryOperator annot_type,
    expr_left :: PExpression annot_type,
    expr_right :: PExpression annot_type,
    expr_annot :: annot_type
  }
  | Assign {
    expr_lvalue :: PLeftValue annot_type,
    expr_subExpr :: PExpression annot_type,
    expr_annot :: annot_type
  }
  | Call {
    expr_func :: FunctionName,
    expr_params :: [PExpression annot_type],
    expr_annot :: annot_type
  }
  deriving (Eq, Read, Show)

data PStatement annot_type
  = Comment {
    stmt_comment :: String,
    stmt_annot :: annot_type
  }
  | Block {
    stmt_stmts :: [PStatement annot_type],
    stmt_annot :: annot_type
  }
  | Expression {
    stmt_expr :: PExpression annot_type,
    stmt_annot :: annot_type
  }
  | If {
    stmt_expr :: PExpression annot_type,
    stmt_then :: PStatement annot_type,
    stmt_annot :: annot_type
  }
  | IfElse {
    stmt_expr :: PExpression annot_type,
    stmt_then :: PStatement annot_type,
    stmt_else :: PStatement annot_type,
    stmt_annot :: annot_type
  }
  | While {
    stmt_expr :: PExpression annot_type,
    stmt_loop :: PStatement annot_type,
    stmt_annot :: annot_type
  }
  | Global {
    stmt_ident :: Identifier,
    stmt_annot :: annot_type
  }
  | Return {
    stmt_retval :: Maybe (PExpression annot_type),
    stmt_annot :: annot_type
  }
  deriving (Eq, Read, Show)

data PTopLevel annot_type
  = Statement {
    toplevel_stmt :: PStatement annot_type,
    toplevel_annot :: annot_type
  }
  | Function {
    toplevel_func :: FunctionName,
    toplevel_params :: [Parameter],
    toplevel_stmts :: [PStatement annot_type],
    toplevel_annot :: annot_type
  }
  deriving (Eq, Read, Show)

data PProgram annot_type
  = Program {
    program_topls :: [PTopLevel annot_type],
    program_annot :: annot_type
  }
  deriving (Eq, Read, Show)

type Identifier = String

type FunctionName = Identifier

type Parameter = Identifier

class AstNode a where
  annot :: a annot -> annot

instance AstNode PLiteral where
  annot = literal_annot

instance AstNode PLeftValue where
  annot = lvalue_annot

instance AstNode PUnaryOperator where
  annot = unOp_annot

instance AstNode PBinaryOperator where
  annot = binOp_annot

instance AstNode PExpression where
  annot = expr_annot

instance AstNode PStatement where
  annot = stmt_annot

instance AstNode PTopLevel where
  annot = toplevel_annot

instance AstNode PProgram where
  annot = program_annot

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
