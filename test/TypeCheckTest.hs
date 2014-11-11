module TypeCheckTest where

import Batsh(parse)
import Batsh.Ast.Typed
import Batsh.Token(LexPos(..))
import Batsh.TypeCheck
import Control.Exception
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
