module ParserTest where

import qualified Batsh
import Batsh.Ast
import Batsh.Token(lpLine, lpColumn, lpStartByte, lpLength, LexPos(LP))
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
  testExpression "3" Literal {expr_literal = Int {literal_int = 3,
    literal_annot = LP {lpLine = 1, lpColumn = 1, lpStartByte = 0,
    lpLength = 1}}, expr_annot = LP {lpLine = 1, lpColumn = 1, lpStartByte = 0,
    lpLength = 1}}
  -- Statement
  testStatement "func(4);" Expression {stmt_expr = Call {expr_func = "func",
    expr_params = [Literal {expr_literal = Int {literal_int = 4, literal_annot =
    LP {lpLine = 1, lpColumn = 6, lpStartByte = 5, lpLength = 1}}, expr_annot =
    LP {lpLine = 1, lpColumn = 6, lpStartByte = 5, lpLength = 1}}], expr_annot =
    LP {lpLine = 1, lpColumn = 1, lpStartByte = 0, lpLength = 7}}, stmt_annot =
    LP {lpLine = 1, lpColumn = 1, lpStartByte = 0, lpLength = 8}}

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
