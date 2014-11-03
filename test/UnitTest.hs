import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit

import qualified GeneratorTest
import qualified LexerTest
import qualified ParserTest
import qualified SymbolTableTest

main :: IO ()
main = defaultMainWithOpts
  [testCase "Lexer"       LexerTest.testLexer,
   testCase "Parser"      ParserTest.testParser,
   testCase "ParseFile"   ParserTest.testParseFile,
   testCase "Generator"   GeneratorTest.testGenerator,
   testCase "SymbolTable" SymbolTableTest.testSymbolTable]
  mempty
