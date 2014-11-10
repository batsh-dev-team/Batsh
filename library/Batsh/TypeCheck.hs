{-#LANGUAGE DeriveDataTypeable#-}
module Batsh.TypeCheck(typeCheck,
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
    Identifier ident pos -> Identifier ident (TypeAnno TVaribale pos)
    ListAccess lvalue expr pos ->
      ListAccess (typeCheck lvalue) (typeCheck expr) (TypeAnno TVaribale pos)

instance TypeCheckable PUnaryOperator where
  typeCheck operator = case operator of
    Not pos -> Not $ TypeAnno TNoType pos
    Negate pos -> Not $ TypeAnno TNoType pos

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
              convertType [TBool, TVaribale] (\_ -> TBool) subExpr'
            Negate _ ->
              convertType [TInt, TFloat, TVaribale] id subExpr'
      checkBinary :: Raw.BinaryOperator -> Raw.Expression -> Raw.Expression
        -> Raw.AstAnnotation -> Expression
      checkBinary operator left right pos =
        Binary operator' left' right' $ TypeAnno inferredType pos
        where
          operator' = typeCheck operator
          left' = typeCheck left
          right' = typeCheck right
          inferredType = case operator' of
            Plus _ -> TInt

instance TypeCheckable PStatement where
  typeCheck stmt = case stmt of
    Expression expr pos -> Expression expr' (TypeAnno TNoType pos)
      where expr' = typeCheck expr

instance TypeCheckable PTopLevel where
  typeCheck topl = case topl of
    Statement stmt pos -> Statement stmt' (TypeAnno TNoType pos)
      where stmt' = typeCheck stmt

instance TypeCheckable PProgram where
  typeCheck (Program topls pos) =
    Program (typeCheckList topls) (TypeAnno TNoType pos)
