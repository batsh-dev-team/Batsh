import Control.Monad
import Data.Monoid
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Map.Strict as SMap

import Batsh
import Batsh.Ast
import Batsh.SymbolTable
import qualified Batsh.Generator
import qualified Batsh.Lexer as Lexer
import qualified Batsh.Parser

testLexer :: Assertion
testLexer = do
  let testSingle str expected = do
      let tokens = Lexer.scanTokens str
      let token = head tokens
      assertEqual "Number of tokens" 1 (length tokens)
      assertEqual (show token) expected token
  testSingle "variable" $ Lexer.Identifier "variable"
  testSingle "//a line comment" $ Lexer.Comment "a line comment"
  testSingle "42" $ Lexer.Int 42
  testSingle "0xFF" $ Lexer.Int 255
  testSingle "3.14" $ Lexer.Float 3.14
  testSingle "1E-8" $ Lexer.Float 1e-8
  testSingle "\"string\\n\"" $ Lexer.String "string\\n"
  testSingle "true" Lexer.TTrue
  testSingle "false" Lexer.TFalse
  testSingle "if" Lexer.If
  testSingle "else" Lexer.Else
  testSingle "while" Lexer.While
  testSingle "function" Lexer.Function
  testSingle "global" Lexer.Global
  testSingle "return" Lexer.Return
  testSingle "!" Lexer.Not
  testSingle ";" Lexer.Semicolon
  testSingle "," Lexer.Comma
  testSingle "+" Lexer.Plus
  testSingle "-" Lexer.Minus
  testSingle "*" Lexer.Multiply
  testSingle "/" Lexer.Divide
  testSingle "%" Lexer.Modulo
  testSingle "++" Lexer.Concat
  testSingle "=" Lexer.Assign
  testSingle "==" Lexer.Equal
  testSingle "!=" Lexer.NotEqual
  testSingle "===" Lexer.ArithEqual
  testSingle "!==" Lexer.ArithNotEqual
  testSingle ">" Lexer.Greater
  testSingle "<" Lexer.Less
  testSingle ">=" Lexer.GreaterEqual
  testSingle "<=" Lexer.LessEqual
  testSingle "&&" Lexer.And
  testSingle "||" Lexer.Or
  testSingle "(" Lexer.LParen
  testSingle ")" Lexer.RParen
  testSingle "[" Lexer.LBrack
  testSingle "]" Lexer.RBrack
  testSingle "{" Lexer.LBrace
  testSingle "}" Lexer.RBrace

testParser :: Assertion
testParser = do
  let testAst :: (Show a, Eq a) => (String -> a) -> String -> a -> Assertion;
      testAst parser code expected =
       assertEqual (show ast) expected ast
       where ast = parser code
  let testProgram = testAst Batsh.Parser.parse
  let testTopLevel = testAst Batsh.Parser.parseTopLevel
  let testStatement = testAst Batsh.Parser.parseStatement
  let testExpression = testAst Batsh.Parser.parseExpression
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
      expected <- parseFromAstFile astFile
      ast <- parseFromFile codeFile
      assertEqual (show ast) expected ast
  forM_ testCases $ \testcase ->
    testParseFile (testCaseDir ++ "/Batsh/" ++ testcase ++ ".batsh")
                  (testCaseDir ++ "/Batsh/" ++ testcase ++ ".ast")

testGenerator :: Assertion
testGenerator = do
  forM_ testCases $ \testcase -> do
    let fileName = testCaseDir ++ "/Batsh/" ++ testcase ++ ".batsh"
    code <- readFile fileName
    let program = parse code
    let formattedCode = Batsh.generateCode program
    assertEqual testcase code formattedCode

testSymbolTable :: Assertion
testSymbolTable = do
  let ast = Batsh.parse "a = 1;"
  let symbolTable = Batsh.createSymbolTable ast
  let expected = SymbolTable (SMap.fromList [("a",("a",SGlobal))])
  assertEqual (show symbolTable) expected symbolTable

main :: IO ()
main = defaultMainWithOpts
  [testCase "Lexer" testLexer,
   testCase "Parser" testParser,
   testCase "ParseFile" testParseFile,
   testCase "Generator" testGenerator,
   testCase "SymbolTable" testSymbolTable]
  mempty
