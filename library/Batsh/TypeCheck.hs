{-#LANGUAGE DeriveDataTypeable#-}
module Batsh.TypeCheck(typeCheck,
                       TypeCheckable,
                       TypeCheckError(..)) where

import qualified Batsh.Ast as Raw
import Batsh.Ast.Typed
import Batsh.Token(LexPos)
import Control.Exception
import Data.Typeable(Typeable)

class AstNode a => TypeCheckable a where
  typeCheck :: a Raw.AstAnnotation -> a TypeAnno

data TypeCheckError
  -- expected_type actual_type position
  = TypeMismatch [Type] Type LexPos
  | AssignFromNoType LexPos
  deriving (Eq, Read, Show, Typeable)

instance Exception TypeCheckError

typeCheckList :: TypeCheckable a => [a Raw.AstAnnotation] -> [a TypeAnno]
typeCheckList list = map typeCheck list

convertType :: AstNode a => [Type] -> (Type -> Type) -> a TypeAnno -> Type
convertType expectedTypes typeConverter node =
  if typeOfNode `elem` expectedTypes then
    typeConverter typeOfNode
  else
    throw $ TypeMismatch expectedTypes typeOfNode (nodePos node)
  where typeOfNode = nodeType node

instance TypeCheckable PLiteral where
  typeCheck literal = case literal of
    Bool bool pos -> Bool bool (TypeAnno TBool pos)
    Int num pos -> Int num (TypeAnno TInt pos)
    Float num pos -> Float num (TypeAnno TFloat pos)
    String str pos -> String str (TypeAnno TString pos)
    List exprs pos -> List (typeCheckList exprs) (TypeAnno TList pos)

instance TypeCheckable PLeftValue where
  typeCheck literal = case literal of
    Identifier ident pos -> Identifier ident (TypeAnno TVariable pos)
    ListAccess lvalue expr pos ->
      ListAccess (typeCheck lvalue) (typeCheck expr) (TypeAnno TVariable pos)

instance TypeCheckable PUnaryOperator where
  typeCheck operator = case operator of
    Not pos -> Not $ TypeAnno TNoType pos
    Negate pos -> Negate $ TypeAnno TNoType pos

instance TypeCheckable PBinaryOperator where
  typeCheck operator = case operator of
    Plus pos -> Plus $ TypeAnno TNoType pos
    Minus pos -> Minus $ TypeAnno TNoType pos
    Multiply pos -> Multiply $ TypeAnno TNoType pos
    Divide pos -> Divide $ TypeAnno TNoType pos
    Modulo pos -> Modulo $ TypeAnno TNoType pos
    Concat pos -> Concat $ TypeAnno TNoType pos
    Equal pos -> Equal $ TypeAnno TNoType pos
    NotEqual pos -> NotEqual $ TypeAnno TNoType pos
    ArithEqual pos -> ArithEqual $ TypeAnno TNoType pos
    ArithNotEqual pos -> ArithNotEqual $ TypeAnno TNoType pos
    Greater pos -> Greater $ TypeAnno TNoType pos
    Less pos -> Less $ TypeAnno TNoType pos
    GreaterEqual pos -> GreaterEqual $ TypeAnno TNoType pos
    LessEqual pos -> LessEqual $ TypeAnno TNoType pos
    And pos -> And $ TypeAnno TNoType pos
    Or pos -> Or $ TypeAnno TNoType pos

instance TypeCheckable PExpression where
  typeCheck expr = case expr of
    LeftValue lvalue pos -> LeftValue lvalue' (TypeAnno (nodeType lvalue') pos)
      where lvalue' = typeCheck lvalue
    Literal literal pos -> Literal literal' (TypeAnno (nodeType literal') pos)
      where literal' = typeCheck literal
    Unary operator subExpr pos -> checkUnary operator subExpr pos
    Binary operator left right pos -> checkBinary operator left right pos
    Assign lvalue subExpr pos -> checkAssign lvalue subExpr pos
    Call func exprs pos -> Call func exprs' (TypeAnno TString pos) -- TODO
      where exprs' = typeCheckList exprs
    where
      checkUnary :: Raw.UnaryOperator -> Raw.Expression -> Raw.AstAnnotation
        -> Expression
      checkUnary operator subExpr pos =
        Unary operator' subExpr' $ TypeAnno inferredType pos
        where
          operator' = typeCheck operator
          subExpr' = typeCheck subExpr
          inferredType = case operator' of
            Not _ ->
              convertType [TBool, TVariable] (\_ -> TBool) subExpr'
            Negate _ ->
              convertType [TInt, TFloat, TVariable] id subExpr'
      checkBinary :: Raw.BinaryOperator -> Raw.Expression -> Raw.Expression
        -> Raw.AstAnnotation -> Expression
      checkBinary operator left right pos =
        Binary operator' left' right' $ TypeAnno inferredType pos
        where
          operator' = typeCheck operator
          left' = typeCheck left
          right' = typeCheck right
          inferredType = TVariable -- TODO
      checkAssign :: Raw.LeftValue -> Raw.Expression -> Raw.AstAnnotation
        -> Expression
      checkAssign lvalue subExpr pos =
        Assign lvalue' subExpr' $ TypeAnno inferredType pos
        where
          lvalue' = typeCheck lvalue
          subExpr' = typeCheck subExpr
          inferredType = case nodeType subExpr' of
            TNoType -> throw $ AssignFromNoType (nodePos subExpr')
            _ -> nodeType subExpr'

instance TypeCheckable PStatement where
  typeCheck stmt = case stmt of
    Comment comment pos -> Comment comment (TypeAnno TNoType pos)
    Block stmts pos -> Block (typeCheckList stmts) (TypeAnno TNoType pos)
    Expression expr pos -> Expression (typeCheck expr) (TypeAnno TNoType pos)
    If expr thenStmt pos ->
      If (typeCheck expr) (typeCheck thenStmt) (TypeAnno TNoType pos)
    IfElse expr thenStmt elseStmt pos ->
      IfElse (typeCheck expr) (typeCheck thenStmt)
             (typeCheck elseStmt) (TypeAnno TNoType pos)
    While expr loopStmt pos ->
      While (typeCheck expr) (typeCheck loopStmt) (TypeAnno TNoType pos)
    Global ident pos -> Global ident (TypeAnno TNoType pos)
    Return (Just expr) pos ->
      Return (Just $ typeCheck expr) (TypeAnno TNoType pos)
    Return Nothing pos ->
      Return Nothing (TypeAnno TNoType pos)

instance TypeCheckable PTopLevel where
  typeCheck topl = case topl of
    Statement stmt pos -> Statement (typeCheck stmt) (TypeAnno TNoType pos)
    Function func params stmts pos -> Function func params stmts' annot
      where
        stmts' = typeCheckList stmts
        annot = TypeAnno TNoType pos -- TODO check type of return value

instance TypeCheckable PProgram where
  typeCheck (Program topls pos) =
    Program (typeCheckList topls) (TypeAnno TNoType pos)
