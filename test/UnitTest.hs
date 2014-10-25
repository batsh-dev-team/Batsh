import Control.Monad
import Data.Monoid
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified BatshAst
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
  testSingle "(" BatshLex.LParen
  testSingle ")" BatshLex.RParen
  testSingle "[" BatshLex.LBrack
  testSingle "]" BatshLex.RBrack
  testSingle "{" BatshLex.LBrace
  testSingle "}" BatshLex.RBrace

testParser :: Assertion
testParser = do
  let ast = BatshParser.parse "3"
  assertEqual (show ast)
    (BatshAst.Expression $ BatshAst.Literal $ BatshAst.Int 3) ast

main :: IO ()
main = defaultMainWithOpts
  [testCase "Lexer" testLexer,
   testCase "Parser" testParser]
  mempty
