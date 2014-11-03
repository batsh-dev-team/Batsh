module GeneratorTest where

import qualified Batsh
import Batsh.Generator
import Control.Monad
import Test.HUnit

testCaseDir = "test/testcase"
testCases = ["arith", "array", "assignment", "block", "command", "comment",
  "exists", "function", "if", "recursion", "string", "while"]

testGenerator :: Assertion
testGenerator = do
  forM_ testCases $ \testcase -> do
    let fileName = testCaseDir ++ "/Batsh/" ++ testcase ++ ".batsh"
    code <- readFile fileName
    let program = Batsh.parse code
    let formattedCode = generateString program
    assertEqual testcase code formattedCode
