import Control.Monad
import Data.Monoid
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Batsh
import BatshAst
import qualified BatshLex
import qualified BatshParser

testLexer :: Assertion
testLexer = do
  let testSingle str expected = do
      let tokens = BatshLex.alexScanTokens str
      let token = head tokens
      assertEqual "Number of tokens" 1 (length tokens)
      assertEqual (show token) expected token
  testSingle "variable" $ BatshLex.Identifier "variable"
  testSingle "//a line comment" $ BatshLex.Comment "a line comment"
  testSingle "42" $ BatshLex.Int 42
  testSingle "0xFF" $ BatshLex.Int 255
  testSingle "3.14" $ BatshLex.Float 3.14
  testSingle "1E-8" $ BatshLex.Float 1e-8
  testSingle "\"string\\n\"" $ BatshLex.String "string\\n"
  testSingle "true" BatshLex.True
  testSingle "false" BatshLex.False
  testSingle "if" BatshLex.If
  testSingle "else" BatshLex.Else
  testSingle "while" BatshLex.While
  testSingle "function" BatshLex.Function
  testSingle "global" BatshLex.Global
  testSingle "return" BatshLex.Return
  testSingle "!" BatshLex.Not
  testSingle ";" BatshLex.Semicolon
  testSingle "," BatshLex.Comma
  testSingle "+" BatshLex.Plus
  testSingle "-" BatshLex.Minus
  testSingle "*" BatshLex.Multiply
  testSingle "/" BatshLex.Divide
  testSingle "%" BatshLex.Modulo
  testSingle "++" BatshLex.Concat
  testSingle "=" BatshLex.Assign
  testSingle "==" BatshLex.Equal
  testSingle "!=" BatshLex.NotEqual
  testSingle "===" BatshLex.ArithEqual
  testSingle "!==" BatshLex.ArithNotEqual
  testSingle ">" BatshLex.Greater
  testSingle "<" BatshLex.Less
  testSingle ">=" BatshLex.GreaterEqual
  testSingle "<=" BatshLex.LessEqual
  testSingle "&&" BatshLex.And
  testSingle "||" BatshLex.Or
  testSingle "(" BatshLex.LParen
  testSingle ")" BatshLex.RParen
  testSingle "[" BatshLex.LBrack
  testSingle "]" BatshLex.RBrack
  testSingle "{" BatshLex.LBrace
  testSingle "}" BatshLex.RBrace

testParser :: Assertion
testParser = do
  let testAst :: (Show a, Eq a) => (String -> a) -> String -> a -> Assertion;
      testAst parser code expected =
       assertEqual (show ast) expected ast
       where ast = parser code
  let testProgram = testAst BatshParser.parse
  let testTopLevel = testAst BatshParser.parseTopLevel
  let testStatement = testAst BatshParser.parseStatement
  let testExpression = testAst BatshParser.parseExpression
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

testParseFile :: Assertion
testParseFile = do
  let testParseFile codeFile astFile = do
      expected <- parseFromAstFile astFile
      ast <- parseFromFile codeFile
      assertEqual (show ast) expected ast
  let testCaseDir = "test/testcase"
  let testCases = ["arith"]
  forM_ testCases $ \testcase ->
    testParseFile (testCaseDir ++ "/Batsh/" ++ testcase ++ ".batsh")
                  (testCaseDir ++ "/BatshAst/" ++ testcase ++ ".parsed")

main :: IO ()
main = defaultMainWithOpts
  [testCase "Lexer" testLexer,
   testCase "Parser" testParser,
   testCase "ParseFile" testParseFile]
  mempty
