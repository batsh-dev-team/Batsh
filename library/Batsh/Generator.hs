module Batsh.Generator where

import qualified Data.ByteString
import Data.ByteString.Lazy(ByteString, putStr, writeFile)
import Data.ByteString.Lazy.Char8(unpack)
import Data.Monoid
import Data.ByteString.Builder(Builder,
                               charUtf8,
                               floatDec,
                               intDec,
                               stringUtf8,
                               toLazyByteString)

import Batsh.Ast

renderLiteral :: Literal -> Builder
renderLiteral literal = case literal of
  Int num _ -> intDec num
  Float num _ -> floatDec num
  String str _ -> mconcat [charUtf8 '"',
                           stringUtf8 str,
                           charUtf8 '"']
  Bool bool _ -> case bool of
    True -> stringUtf8 "true"
    False -> stringUtf8 "false"
  List list _ -> mconcat [stringUtf8 "[",
                          renderExpressions list,
                          stringUtf8 "]"]

renderLeftValue :: LeftValue -> Builder
renderLeftValue lvalue = case lvalue of
  Identifier ident _ -> stringUtf8 ident
  ListAccess (lvalue, expr) _ -> mconcat [renderLeftValue lvalue,
                                          stringUtf8 "[",
                                          renderExpression expr,
                                          stringUtf8 "]"]

-- Render a subexpression. Add parenthesis if and only if necessary.
renderSubExpression :: (Operator a) => a -> Expression -> Builder
renderSubExpression operator subExpr =
  let rendered = renderExpression subExpr in
  let renderedWithParen = mconcat [charUtf8 '(', rendered, charUtf8 ')'] in
  case subExpr of
    -- if subexpression is a binary expression and the precedence of operator is
    -- lower, then add (). E.g. + is less precedent than *.
    Binary (subOperator, _, _) _
      | precedence subOperator < precedence operator ->
         renderedWithParen
    Unary (subOperator, _) _
      | precedence subOperator < precedence operator ->
         renderedWithParen
    _ -> rendered

renderUnary :: (UnaryOperator, Expression) -> Builder
renderUnary (operator, expr) =
  mconcat [stringUtf8 $ operatorStr operator,
           renderSubExpression operator expr]

renderBinary :: (BinaryOperator, Expression, Expression) -> Builder
renderBinary (operator, left, right) =
  mconcat [renderSubExpression operator left,
           charUtf8 ' ',
           stringUtf8 $ operatorStr operator,
           charUtf8 ' ',
           renderSubExpression operator right]

renderExpression :: Expression -> Builder
renderExpression expr = case expr of
  LeftValue lvalue _ -> renderLeftValue lvalue
  Literal literal _ -> renderLiteral literal
  Unary unary _ -> renderUnary unary
  Binary binary _ -> renderBinary binary
  Assign (lvalue, expr) _ -> mconcat [renderLeftValue lvalue,
                                      stringUtf8 " = ",
                                      renderExpression expr]
  Call (ident, exprs) _ -> mconcat [stringUtf8 ident,
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

renderIndention :: Int -> Builder
renderIndention level = stringUtf8 (take level $ repeat ' ')

renderBlock :: [Statement] -> Int -> Builder
renderBlock stmts level = mconcat
  [stringUtf8 "{\n",
   renderSeparateList stmts "\n" $
    \stmt -> renderStatementIndent stmt (level + 2) False,
   stringUtf8 "\n",
   renderIndention level,
   stringUtf8 "}"
  ]

renderStatementIndent :: Statement -> Int -> Bool -> Builder
renderStatementIndent stmt level isClause =
  if isClause then
    renderedStmt
  else
    mconcat [renderIndention level, renderedStmt]
  where
    renderedStmt = case stmt of
      Comment comment _ ->
        mconcat [stringUtf8 "//",
                 stringUtf8 comment]
      Block stmts _ -> renderBlock stmts level
      Expression expr _ -> withSemicolon $ renderExpression expr
      If (expr, stmt) _ ->
        mconcat [stringUtf8 "if (",
                 renderExpression expr,
                 stringUtf8 ") ",
                 renderClause stmt]
      IfElse (expr, thenStmt, elseStmt) _ ->
        mconcat [stringUtf8 "if (",
                 renderExpression expr,
                 stringUtf8 ") ",
                 renderClause thenStmt,
                 stringUtf8 " else ",
                 renderClause elseStmt]
      While (expr, stmt) _ ->
        mconcat [stringUtf8 "while (",
                 renderExpression expr,
                 stringUtf8 ") ",
                 renderClause stmt]
      Global ident _ -> withSemicolon $
        mconcat [stringUtf8 "global ", stringUtf8 ident]
      Return (Just expr) _ -> withSemicolon $
        mconcat [stringUtf8 "return ", renderExpression expr]
      Return Nothing _ -> withSemicolon $ stringUtf8 "return"
      where
        withSemicolon builder = mconcat [builder, charUtf8 ';'];
        renderClause stmt = renderStatementIndent stmt level True

renderStatement :: Statement -> Builder
renderStatement stmt = renderStatementIndent stmt 0 False

renderTopLevel :: TopLevel -> Builder
renderTopLevel toplevel = case toplevel of
  Statement stmt _ -> renderStatement stmt
  Function (name, params, stmts) _ ->
    mconcat [stringUtf8 "function ",
             stringUtf8 name,
             charUtf8 '(',
             renderSeparateList params ", " stringUtf8,
             stringUtf8 ") ",
             renderBlock stmts 0]

renderProgram :: Program -> Builder
renderProgram (Program program _) =
  mconcat [renderSeparateList program "\n" renderTopLevel,
           charUtf8 '\n']

generateByteString :: Program -> ByteString
generateByteString program = toLazyByteString $ renderProgram program

generateString :: Program -> String
generateString program = unpack $ generateByteString program

printToStdout :: Program -> IO ()
printToStdout program =
  Data.ByteString.Lazy.putStr $ generateByteString program

printToFile :: Program -> FilePath -> IO ()
printToFile program filename = Data.ByteString.Lazy.writeFile filename code
  where code = generateByteString program
