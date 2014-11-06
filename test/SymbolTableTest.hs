module SymbolTableTest where

import qualified Batsh
import Batsh.Ast
import Batsh.SymbolTable
import Control.Exception
import Control.Monad
import qualified Data.Map.Strict as SMap
import Prelude hiding(catch)
import Test.HUnit

testSymbolTable :: Assertion
testSymbolTable = do
  let actual = symbolTable "a = 1;"
  let expected = symbolTable "a = 1;"
  assertEqual (show actual) expected actual
  catch (print $ symbolTable "function a(){} function a(){}")
        (handler $ "Redefinition of function 'a'" ++ errorSuffix)
  catch (print $ symbolTable "function x(a,b,a){}")
        (handler $ "Duplicated parameter 'a' in function 'x'" ++ errorSuffix)
  catch (print $ symbolTable "f = 1; function f(){}")
        (handler $ "Symbol conflict: 'f' is a function" ++ errorSuffix)
  where
    handler :: String -> SomeException -> IO ()
    handler expected ex = assertEqual (show ex) expected (show ex)
    errorSuffix = " when creating symbol table"
    symbolTable :: String -> SymbolTable
    symbolTable code = Batsh.createSymbolTable $ Batsh.parse code

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
