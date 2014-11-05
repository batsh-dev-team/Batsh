{-# LANGUAGE FlexibleInstances #-}
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

import Batsh.Ast.Poly

class Renderable a where
  render :: a -> Builder

renderSeparateList :: [a] -> String -> (a -> Builder) -> Builder
renderSeparateList list separator renderer = case list of
  [] -> mempty
  [elem] -> renderer elem
  (elem : rest) -> mconcat [renderer elem,
                            render separator,
                            renderSeparateList rest separator renderer]

renderIndention :: Int -> Builder
renderIndention level = render (take level $ repeat ' ')

instance Renderable Char where
  render char = charUtf8 char

instance Renderable String where
  render str = stringUtf8 str

instance Renderable (PLiteral a) where
  render literal = case literal of
    Int num _ -> intDec num
    Float num _ -> floatDec num
    String str _ -> mconcat [render '"',
                             render str,
                             render '"']
    Bool bool _ -> case bool of
      True -> render "true"
      False -> render "false"
    List list _ -> mconcat [render "[",
                            render list,
                            render "]"]

instance Renderable (PLeftValue a) where
  render lvalue = case lvalue of
    Identifier ident _ -> render ident
    ListAccess (lvalue, expr) _ -> mconcat [render lvalue,
                                            render "[",
                                            render expr,
                                            render "]"]

instance Renderable (PExpression a) where
  render expr = case expr of
    LeftValue lvalue _ -> render lvalue
    Literal literal _ -> render literal
    Unary unary _ -> renderUnary unary
    Binary binary _ -> renderBinary binary
    Assign (lvalue, expr) _ -> mconcat [render lvalue,
                                        render " = ",
                                        render expr]
    Call (ident, exprs) _ -> mconcat [render ident,
                                      render '(',
                                      render exprs,
                                      render ')']
    where
    -- Render a subexpression. Add parenthesis if and only if necessary.
    renderSubExpression :: (Operator a) => a -> (PExpression b) -> Builder
    renderSubExpression operator subExpr =
      let rendered = render subExpr in
      let renderedWithParen = mconcat [render '(', rendered, render ')'] in
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

    renderUnary :: (PUnaryOperator a, PExpression a) -> Builder
    renderUnary (operator, expr) =
      mconcat [render $ operatorStr operator,
               renderSubExpression operator expr]

    renderBinary :: (PBinaryOperator a, PExpression a, PExpression a) -> Builder
    renderBinary (operator, left, right) =
      mconcat [renderSubExpression operator left,
               render ' ',
               render $ operatorStr operator,
               render ' ',
               renderSubExpression operator right]

instance Renderable [PExpression a] where
  render exprs = renderSeparateList exprs ", " render

instance Renderable (PStatement a) where
  render stmt = renderStatementIndent stmt 0 False

renderBlock :: [PStatement a] -> Int -> Builder
renderBlock stmts level = mconcat
  [render "{\n",
   renderSeparateList stmts "\n" $
    \stmt -> renderStatementIndent stmt (level + 2) False,
   render "\n",
   renderIndention level,
   render "}"
  ]

renderStatementIndent :: PStatement a -> Int -> Bool -> Builder
renderStatementIndent stmt level isClause =
  if isClause then
    renderedStmt
  else
    mconcat [renderIndention level, renderedStmt]
  where
    renderedStmt = case stmt of
      Comment comment _ ->
        mconcat [render "//",
                 render comment]
      Block stmts _ -> renderBlock stmts level
      Expression expr _ -> withSemicolon $ render expr
      If (expr, stmt) _ ->
        mconcat [render "if (",
                 render expr,
                 render ") ",
                 renderClause stmt]
      IfElse (expr, thenStmt, elseStmt) _ ->
        mconcat [render "if (",
                 render expr,
                 render ") ",
                 renderClause thenStmt,
                 render " else ",
                 renderClause elseStmt]
      While (expr, stmt) _ ->
        mconcat [render "while (",
                 render expr,
                 render ") ",
                 renderClause stmt]
      Global ident _ -> withSemicolon $
        mconcat [render "global ", render ident]
      Return (Just expr) _ -> withSemicolon $
        mconcat [render "return ", render expr]
      Return Nothing _ -> withSemicolon $ render "return"
      where
        withSemicolon builder = mconcat [builder, render ';'];
        renderClause stmt = renderStatementIndent stmt level True

instance Renderable (PTopLevel a) where
  render toplevel = case toplevel of
    Statement stmt _ -> render stmt
    Function (name, params, stmts) _ ->
      mconcat [render "function ",
               render name,
               render '(',
               renderSeparateList params ", " render,
               render ") ",
               renderBlock stmts 0]

instance Renderable (PProgram a) where
  render (Program program _) =
    mconcat [renderSeparateList program "\n" render,
             render '\n']

generateByteString :: (PProgram a) -> ByteString
generateByteString program = toLazyByteString $ render program

generateString :: (PProgram a) -> String
generateString program = unpack $ generateByteString program

printToStdout :: (PProgram a) -> IO ()
printToStdout program =
  Data.ByteString.Lazy.putStr $ generateByteString program

printToFile :: (PProgram a) -> FilePath -> IO ()
printToFile program filename = Data.ByteString.Lazy.writeFile filename code
  where code = generateByteString program
