module ParserTest where

import qualified Batsh
import Batsh.Ast
import Batsh.Parser
import Control.Monad
import Test.HUnit

testParser :: Assertion
testParser = do
  let testAst :: (Show a, Eq a) => (String -> a) -> String -> a -> Assertion;
      testAst parser code expected =
       assertEqual (show ast) expected ast
       where ast = parser code
  let testProgram = testAst parse
  let testTopLevel = testAst parseTopLevel
  let testStatement = testAst parseStatement
  let testExpression = testAst parseExpression
  -- Expression
  testExpression "3" (Literal (Int 3 ())())
  -- Statement
  testStatement "func(4);" (Expression (Call ("func",[Literal (Int 4 ()) ()]) ()) ())

testCaseDir = "test/testcase"
testCases = ["arith", "array", "assignment", "block", "command", "comment",
  "exists", "function", "if", "recursion", "string", "while"]

testParseFile :: Assertion
testParseFile = do
  let testParseFile codeFile astFile = do
      expected <- Batsh.parseFromAstFile astFile
      ast <- Batsh.parseFromFile codeFile
      assertEqual (show ast) expected ast
  forM_ testCases $ \testcase ->
    testParseFile (testCaseDir ++ "/Batsh/" ++ testcase ++ ".batsh")
                  (testCaseDir ++ "/Batsh/" ++ testcase ++ ".ast")
