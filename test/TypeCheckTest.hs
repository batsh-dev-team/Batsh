module TypeCheckTest where

import Batsh(parse, parseFromFile)
import Batsh.Ast.Typed
import Batsh.Token(LexPos(..))
import Batsh.TypeCheck
import Control.Exception
import Control.Monad
import Prelude hiding(catch)
import Test.HUnit

testTypeCheck :: Assertion
testTypeCheck = do
  assertTypeCheck "3;" $ Program {program_topls = [Statement {toplevel_stmt =
    Expression {stmt_expr = Literal {expr_literal = Int {literal_int = 3,
    literal_annot = TypeAnno TInt (LP {lpLine = 1, lpColumn = 1,
    lpStartByte = 0, lpLength = 1})}, expr_annot = TypeAnno TInt (LP {
    lpLine = 1, lpColumn = 1, lpStartByte = 0, lpLength = 1})},
    stmt_annot = TypeAnno TNoType (LP {lpLine = 1, lpColumn = 1,
    lpStartByte = 0, lpLength = 2})}, toplevel_annot = TypeAnno TNoType (
    LP {lpLine = 1, lpColumn = 1, lpStartByte = 0, lpLength = 2})}],
    program_annot = TypeAnno TNoType (LP {lpLine = 1, lpColumn = 1,
    lpStartByte = 0, lpLength = 2})}
  catch (print $ typed "!3;")
        (handler $ TypeMismatch [TBool,TVariable] TInt (LP {lpLine = 1,
        lpColumn = 2, lpStartByte = 1, lpLength = 1}))
  return ()
  where
    typed :: String -> Program
    typed code = typeCheck $ parse code
    assertTypeCheck :: String -> Program -> Assertion
    assertTypeCheck code expected =
      let typedAst = typed code in
      assertEqual (show typedAst) expected typedAst
    handler :: (Eq a, Exception a) => a -> a -> IO ()
    handler expected ex = assertEqual (show ex) expected ex

testCaseDir = "test/testcase"
testCases = ["arith", "array", "assignment", "block", "command", "comment",
  "exists", "function", "if", "recursion", "string", "while"]

testTypeCheckFile :: Assertion
testTypeCheckFile = do
  let testParseFile codeFile typedAstFile = do
      text <- readFile typedAstFile
      let expected = read text :: Program
      ast <- parseFromFile codeFile
      let typed = typeCheck ast
      assertEqual (show typedAstFile) expected typed
  forM_ testCases $ \testcase ->
    testParseFile (testCaseDir ++ "/Batsh/" ++ testcase ++ ".batsh")
                  (testCaseDir ++ "/Batsh/" ++ testcase ++ ".typed")
