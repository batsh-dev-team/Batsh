module Batsh.Generator where

import qualified Data.ByteString
import Data.ByteString.Lazy(ByteString, putStr, writeFile)
import Data.Monoid
import Data.ByteString.Builder(Builder,
                               charUtf8,
                               floatDec,
                               intDec,
                               stringUtf8,
                               toLazyByteString)
import Data.Word

import Batsh.Ast

renderLiteral :: Literal -> Builder
renderLiteral literal = case literal of
  Int num -> intDec num
  Float num -> floatDec num
  String str -> mconcat [charUtf8 '"',
                         stringUtf8 str,
                         charUtf8 '"']
  Bool bool -> case bool of
    True -> stringUtf8 "true"
    False -> stringUtf8 "false"
  List list -> mconcat [stringUtf8 "[",
                        renderExpressions list,
                        stringUtf8 "]"]

renderLeftValue :: LeftValue -> Builder
renderLeftValue lvalue = case lvalue of
  Identifier ident -> stringUtf8 ident
  ListAccess (lvalue, expr) -> mconcat [renderLeftValue lvalue,
                                        stringUtf8 "[",
                                        renderExpression expr,
                                        stringUtf8 "]"]

renderUnary :: (UnaryOperator, Expression) -> Builder
renderUnary (operator, expr) =
  mconcat [charUtf8 operatorString, renderExpression expr]
  where operatorString =
          case operator of
            Not -> '!'
            Negate -> '-'

renderBinary :: (BinaryOperator, Expression, Expression) -> Builder
renderBinary (operator, left, right) =
  mconcat [renderExpression left,
           stringUtf8 operatorString,
           renderExpression right]
  where operatorString =
          case operator of
            Plus -> "+"
            Minus -> "-"
            Multiply -> "*"
            Divide -> "/"
            Modulo -> "%"
            Concat -> "++"
            Equal -> "=="
            NotEqual -> "!="
            ArithEqual -> "==="
            ArithNotEqual -> "!=="
            Greater -> ">"
            Less -> "<"
            GreaterEqual -> ">="
            LessEqual -> "<="
            And -> "&&"
            Or -> "||"

renderExpression :: Expression -> Builder
renderExpression expr = case expr of
  LeftValue lvalue -> renderLeftValue lvalue
  Literal literal -> renderLiteral literal
  Unary unary -> renderUnary unary
  Binary binary -> renderBinary binary
  Assign (lvalue, expr) -> mconcat [renderLeftValue lvalue,
                                    stringUtf8 " = ",
                                    renderExpression expr]
  Call (ident, exprs) -> mconcat [stringUtf8 ident,
                                  charUtf8 '(',
                                  renderExpressions exprs,
                                  charUtf8 ')']

renderSeparateList :: [a] -> String -> (a -> Builder) -> Builder
renderSeparateList list separator renderer = case list of
  [] -> mempty
  [elem] -> renderer elem
  (elem : rest) -> mconcat [renderer elem,
                            stringUtf8 separator,
                            renderSeparateList rest separator renderer]

renderExpressions :: [Expression] -> Builder
renderExpressions exprs = renderSeparateList exprs ", " renderExpression

renderBlock :: [Statement] -> Word -> Builder
renderBlock stmts level = mconcat
  [stringUtf8 "{",
   mconcat $ map (\stmt-> renderStatementIndent stmt $ level + 2) stmts,
   stringUtf8 "}"
  ]

renderStatementIndent :: Statement -> Word -> Builder
renderStatementIndent stmt level = case stmt of
  Comment comment -> mconcat [stringUtf8 "// ",
                              stringUtf8 comment]
  Block stmts -> renderBlock stmts level
  Expression expr -> withSemicolon $ renderExpression expr
  If (expr, stmt) -> mconcat [stringUtf8 "if (",
                              renderExpression expr,
                              stringUtf8 ") ",
                              renderStatement stmt]
  IfElse (expr, thenStmt, elseStmt) -> mconcat [stringUtf8 "if (",
                                                renderExpression expr,
                                                stringUtf8 ") ",
                                                renderStatement thenStmt,
                                                stringUtf8 " else ",
                                                renderStatement elseStmt]
  While (expr, stmt) -> mconcat [stringUtf8 "while (",
                                 renderExpression expr,
                                 stringUtf8 ") ",
                                 renderStatement stmt]
  Global ident -> withSemicolon $ stringUtf8 ident
  Return (Just expr) -> withSemicolon $ mconcat [stringUtf8 "return ",
                                                 renderExpression expr]
  Return Nothing -> withSemicolon $ stringUtf8 "return"
  where withSemicolon builder = mconcat [builder, charUtf8 ';']

renderStatement :: Statement -> Builder
renderStatement stmt = renderStatementIndent stmt (0 :: Word)

renderTopLevel :: TopLevel -> Builder
renderTopLevel toplevel = case toplevel of
  Statement stmt -> renderStatement stmt

renderProgram :: Program -> Builder
renderProgram program = renderSeparateList program ", " renderTopLevel

generateByteString :: Program -> ByteString
generateByteString program = toLazyByteString $ renderProgram program

generateString :: Program -> String
generateString = show . generateByteString

printToStdout :: Program -> IO ()
printToStdout program = do
  Data.ByteString.Lazy.putStr $ generateByteString program
  putChar '\n'

printToFile :: Program -> FilePath -> IO ()
printToFile program filename = Data.ByteString.Lazy.writeFile filename code
  where code = generateByteString program
