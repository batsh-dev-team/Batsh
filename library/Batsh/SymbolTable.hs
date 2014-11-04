module Batsh.SymbolTable(create,
                         lookup,
                         SymbolTable) where

import Batsh.Ast
import qualified Data.List as List
import qualified Data.Map.Strict as SMap
import Prelude hiding(lookup)

newtype SymbolTable = SymbolTable MapScopeTable
  deriving (Eq, Read, Show)

-- (key: Scope, value: Table)
type MapScopeTable = SMap.Map Scope Table

-- table: (key: Identifier, value: Symbol), parentScope
newtype Table = Table (MapIdentifierSymbol, Maybe Scope)
  deriving (Eq, Read, Show)

type MapIdentifierSymbol = SMap.Map Identifier Symbol

type Symbol = (Identifier, SymbolType, Scope)

data SymbolType = STVariable | STParameter | STFunction
  deriving (Eq, Read, Show)

data Scope = SGlobal | SFunction FunctionName | SUnnamedBlock BlockID
  deriving (Eq, Read, Show, Ord)

type BlockID = String

lookupScope :: SymbolTable -> Scope -> Maybe Table
lookupScope (SymbolTable symbolTable) scope = SMap.lookup scope symbolTable

lookup :: SymbolTable -> Scope -> Identifier -> Maybe Symbol
lookup symbolTable scope ident =
  case lookupScope symbolTable scope of
    Just (Table (table, parentScope)) ->
      case SMap.lookup ident table of
        Just symbol -> Just symbol -- Symbol found
        Nothing -> -- Recursively lookup parent scopes
          case parentScope of
            Just scope -> Batsh.SymbolTable.lookup symbolTable scope ident
            Nothing -> Nothing -- Already the top scope (SGlobal)
    Nothing -> Nothing -- scope does not exist

create :: Program -> SymbolTable
create (Program program) =
  SymbolTable $ SMap.fromList $ (SGlobal, globalTable) : functionTables
  where
  (functions, topLevelStmts) = List.partition (\topl -> case topl of
    Function _ -> True
    Statement _ -> False
    ) program;
  stmts = map (\(Statement stmt) -> stmt) topLevelStmts
  -- Add function definitions to global table
  functionDefTable = Table (foldl addFuncDef SMap.empty functions, Nothing)
  -- Add global statements to global table
  globalTable = processStatements functionDefTable SGlobal stmts
  -- Create symbol tables for every function
  functionTables :: [(Scope, Table)]
  functionTables = map processFunction functions
  addFuncDef :: MapIdentifierSymbol -> TopLevel -> MapIdentifierSymbol
  addFuncDef table (Function (func, _, _)) =
    let scope = SGlobal in -- functions can be defined only in the global scope
    let symbol = (func, STFunction, scope) in
    if not (SMap.member func table) then
      SMap.insert func symbol table
    else
      symbolTableError $ "Redefinition of function '" ++ func ++ "'"

processFunction :: TopLevel -> (Scope, Table)
processFunction (Function (func, params, stmts)) =
  (scope, table)
  where
  scope = SFunction func;
  parentScope = SGlobal;
  -- Add params
  paramTable = Table (foldl addFuncParam SMap.empty params, Just parentScope);
  addFuncParam :: MapIdentifierSymbol -> Parameter -> MapIdentifierSymbol;
  addFuncParam table param =
    SMap.alter addParam param table
    where
    addParam original = case original of
      Nothing -> Just (param, STParameter, scope)
      Just _ -> symbolTableError $
        "Duplicated parameter '" ++ param ++ "' in function '" ++ func ++ "'"
  -- Add statements
  table = processStatements paramTable scope stmts;

processStatement :: Table -> Scope -> Statement -> Table
processStatement table scope stmt = case stmt of
  Block stmts -> processStatements table scope stmts -- TODO block scope
  Expression expr -> processExpression table scope expr
  If (expr, subStmt) ->
    let exprTable = processExpression table scope expr in
    processStatement exprTable scope subStmt
  IfElse (expr, thenStmt, elseStmt) ->
    let exprTable = processExpression table scope expr in
    let thenTable = processStatement exprTable scope thenStmt in
    processStatement thenTable scope elseStmt
  While (expr, subStmt) ->
    let exprTable = processExpression table scope expr in
    processStatement exprTable scope subStmt
  Global ident -> processIdentifier table SGlobal ident
  Return (Just expr) -> processExpression table scope expr
  _ -> table

processStatements :: Table -> Scope -> [Statement] -> Table
processStatements table scope stmts =
  foldl (\table stmt -> processStatement table scope stmt) table stmts

processExpression :: Table -> Scope -> Expression -> Table
processExpression table scope expr = case expr of
  Assign (lvalue, subExpr) -> processLeftValue table scope lvalue
  Unary (_, subExpr) -> processExpression table scope subExpr
  Binary (_, left, right) ->
    let exprTable = processExpression table scope left in
    processExpression exprTable scope right
  _ -> table

processLeftValue :: Table -> Scope -> LeftValue -> Table
processLeftValue table scope lvalue = case lvalue of
  Identifier ident -> processIdentifier table scope ident
  ListAccess (lvalue, subExpr) ->
    let lvalueTable = processLeftValue table scope lvalue in
    processExpression lvalueTable scope subExpr

processIdentifier :: Table -> Scope -> Identifier -> Table
processIdentifier (Table (table, parentScope)) scope ident =
  Table (SMap.alter update ident table, parentScope)
  where
    update original = case original of
      -- New symbol (unseen before)
      Nothing -> newSymbol
      -- If there is a parameter or variable having the same name, then keep the
      -- original one.
      Just (_, STParameter, _) -> original
      Just (_, STVariable, _) -> original
      -- Treat as conflict if there is a function having the same name.
      Just (_, STFunction, _) ->
        symbolTableError $ "Symbol conflict: '" ++ ident ++ "' is a function"
      where newSymbol = Just (ident, STVariable, scope)

symbolTableError message = error $  message ++ " when creating symbol table"
