module Batsh.SymbolTable where

import qualified Data.Map.Strict as SMap

import Batsh.Ast

newtype SymbolTable = SymbolTable Table
  deriving (Eq, Read, Show)

type Table = SMap.Map Identifier Symbol

type Symbol = (Identifier, Scope)

data Scope = SGlobal | SFunction FunctionName
  deriving (Eq, Read, Show)

create :: Program -> SymbolTable
create (Program program) =
  SymbolTable $ SMap.unions $ map processToplevel program

processToplevel :: TopLevel -> Table
processToplevel topLevel = case topLevel of
  Statement stmt -> processStatement SMap.empty SGlobal stmt
  Function func -> SMap.empty

processStatement :: Table -> Scope -> Statement -> Table
processStatement table scope stmt = case stmt of
  Block stmts -> processStatements table scope stmts
  Expression expr -> processExpression table scope expr
  If (expr, stmt) -> table -- FIXME
  IfElse (expr, thenStmt, elseStmt) -> table -- FIXME
  While (expr, stmt) -> table -- FIXME
  Global ident -> processIdentifier table SGlobal ident
  Return (Just expr) -> table -- FIXME
  _ -> table

processStatements :: Table -> Scope -> [Statement] -> Table
processStatements table scope stmts =
  foldl (\table stmt -> processStatement table scope stmt) table stmts

processExpression :: Table -> Scope -> Expression -> Table
processExpression table scope expr = case expr of
  Assign (lvalue, subExpr) -> processLeftValue table scope lvalue
  -- FIXME

processLeftValue :: Table -> Scope -> LeftValue -> Table
processLeftValue table scope lvalue = case lvalue of
  Identifier ident -> processIdentifier table scope ident
  ListAccess (lvalue, subExpr) -> table -- FIXME

processIdentifier :: Table -> Scope -> Identifier -> Table
processIdentifier table scope ident =
  SMap.alter update ident table
  where
    update original = case original of
      Nothing -> newSymbol
      Just (_, SFunction _) -> newSymbol
      Just (_, SGlobal) -> original
      where newSymbol = Just (ident, scope)
