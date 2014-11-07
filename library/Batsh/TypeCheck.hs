module Batsh.TypeCheck where

import qualified Batsh.Ast as Raw
import Batsh.Ast.Typed

class AstNode a => TypeCheckable a where
  typeCheck :: a Raw.AstAnnotation -> a TypeAnno

typeCheckList :: TypeCheckable a => [a Raw.AstAnnotation] -> [a TypeAnno]
typeCheckList list = map typeCheck list

instance TypeCheckable PLiteral where
  typeCheck literal = case literal of
    Bool bool pos -> Bool bool (TypeAnno TBool pos)
    Int num pos -> Int num (TypeAnno TInt pos)
    Float num pos -> Float num (TypeAnno TFloat pos)
    String str pos -> String str (TypeAnno TString pos)
    List exprs pos -> List (typeCheckList exprs) (TypeAnno TList pos)

-- All variables are considered as String
instance TypeCheckable PLeftValue where
  typeCheck literal = case literal of
    Identifier ident pos -> Identifier ident (TypeAnno TString pos)
    ListAccess lvalue expr pos ->
      ListAccess (typeCheck lvalue) (typeCheck expr) (TypeAnno TString pos)

instance TypeCheckable PExpression where
  typeCheck expr = case expr of
    LeftValue lvalue pos -> LeftValue (typeCheck lvalue) (TypeAnno TString pos)
    Literal literal pos -> Literal (typeCheck literal) (TypeAnno TString pos)
