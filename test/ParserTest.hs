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
  testExpression "3" (Literal $ Int 3)
  testExpression "[4.2 + 3, \"str\", []]" (Literal $ List [Binary (Plus,
    Literal $ Float 4.2 , Literal $ Int 3), Literal $ String "str",
    Literal $ List []])
  testExpression "a+2+9*4e2 > 5 || true && 4 != \"str\"" (Binary (Or, Binary (
    Greater, Binary (Plus, Binary (Plus, LeftValue (Identifier "a"), Literal (
    Int 2)), Binary (Multiply, Literal (Int 9), Literal (Float 400.0))), Literal
    (Int 5)), Binary (And, Literal (Bool True), Binary (NotEqual, Literal (Int
    4), Literal (String "str")))))
  -- Statement
  testStatement "func(4);" (Expression $ Call ("func", [Literal $ Int 4]))
  testStatement "if (1) if (2) {true;} else {}" (If (Literal (Int 1), IfElse (
    Literal (Int 2), Block [Expression (Literal (Bool True))], Block [])))

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
