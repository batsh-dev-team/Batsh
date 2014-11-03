module SymbolTableTest where

import qualified Batsh
import Batsh.Ast
import Batsh.SymbolTable
import qualified Data.Map.Strict as SMap
import Test.HUnit

testSymbolTable :: Assertion
testSymbolTable = do
  let ast = Batsh.parse "a = 1;"
  let symbolTable = Batsh.createSymbolTable ast
  let expected = SymbolTable (SMap.fromList [("a",("a",SGlobal))])
  assertEqual (show symbolTable) expected symbolTable
