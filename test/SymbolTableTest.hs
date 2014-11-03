module SymbolTableTest where

import qualified Batsh
import Batsh.Ast
import Batsh.SymbolTable
import Control.Monad
import qualified Data.Map.Strict as SMap
import Test.HUnit

testSymbolTable :: Assertion
testSymbolTable = do
  let ast = Batsh.parse "a = 1;"
  let symbolTable = Batsh.createSymbolTable ast
  let expected = Batsh.createSymbolTable ast
  assertEqual (show symbolTable) expected symbolTable

testCaseDir = "test/testcase"
testCases = ["arith", "array", "assignment", "block", "command", "comment",
  "exists", "function", "if", "recursion", "string", "while"]

testSymbolTableFile :: Assertion
testSymbolTableFile = do
  let testParseFile codeFile symbolFile = do
      symbolsText <- readFile symbolFile
      let expected = read symbolsText :: SymbolTable
      ast <- Batsh.parseFromFile codeFile
      let symbols = Batsh.createSymbolTable ast
      assertEqual (show symbolFile) expected symbols
  forM_ testCases $ \testcase ->
    testParseFile (testCaseDir ++ "/Batsh/" ++ testcase ++ ".batsh")
                  (testCaseDir ++ "/Batsh/" ++ testcase ++ ".symbols")
