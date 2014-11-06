{-# LANGUAGE FlexibleInstances #-}
module Batsh.Generator where

import Batsh.Ast.Poly
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

class Renderable a where
  render :: Renderable a => a -> Builder

renderSeparateList :: [a] -> String -> (a -> Builder) -> Builder
renderSeparateList list separator renderer = case list of
  [] -> mempty
  [elem] -> renderer elem
  (elem : rest) ->
    build (renderer elem) separator (renderSeparateList rest separator renderer)

renderIndention :: Int -> Builder
renderIndention level = render (take level $ repeat ' ')

instance Renderable Builder where
  render = id

instance Renderable Char where
  render = charUtf8

instance Renderable String where
  render = stringUtf8

instance Renderable Int where
  render = intDec

instance Renderable Float where
  render = floatDec

instance Renderable (PLiteral a) where
  render literal = case literal of
    Int num _ -> intDec num
    Float num _ -> floatDec num
    String str _ -> build '"' str '"'
    Bool bool _ -> case bool of
      True -> render "true"
      False -> render "false"
    List list _ -> build "["  list "]"

instance Renderable (PLeftValue a) where
  render lvalue = case lvalue of
    Identifier ident _ -> render ident
    ListAccess (lvalue, expr) _ -> build lvalue "[" expr "]"

instance Renderable (PExpression a) where
  render expr = case expr of
    LeftValue lvalue _ -> render lvalue
    Literal literal _ -> render literal
    Unary unary _ -> renderUnary unary
    Binary binary _ -> renderBinary binary
    Assign (lvalue, expr) _ -> build lvalue " = " expr
    Call (ident, exprs) _ -> build ident '(' exprs ')'
    where
    -- Render a subexpression. Add parenthesis if and only if necessary.
    renderSubExpression :: (Operator a) => a -> (PExpression b) -> Builder
    renderSubExpression operator subExpr =
      let renderedWithParen = build '(' subExpr ')' in
      case subExpr of
        -- if subexpression is a binary expression and the precedence of operator is
        -- lower, then add (). E.g. + is less precedent than *.
        Binary (subOperator, _, _) _
          | precedence subOperator < precedence operator ->
             renderedWithParen
        Unary (subOperator, _) _
          | precedence subOperator < precedence operator ->
             renderedWithParen
        _ -> render subExpr

    renderUnary :: (PUnaryOperator a, PExpression a) -> Builder
    renderUnary (operator, expr) =
      build (operatorStr operator) (renderSubExpression operator expr)

    renderBinary :: (PBinaryOperator a, PExpression a, PExpression a) -> Builder
    renderBinary (operator, left, right) =
      build (renderSubExpression operator left) ' '
            (operatorStr operator) ' '
            (renderSubExpression operator right)

instance Renderable [PExpression a] where
  render exprs = renderSeparateList exprs ", " render

instance Renderable (PStatement a) where
  render stmt = renderStatementIndent stmt 0 False

renderBlock :: [PStatement a] -> Int -> Builder
renderBlock stmts level = build "{\n"
   (renderSeparateList stmts "\n" $
    \stmt -> renderStatementIndent stmt (level + 2) False)
   "\n" (renderIndention level) "}"

renderStatementIndent :: PStatement a -> Int -> Bool -> Builder
renderStatementIndent stmt level isClause =
  if isClause then
    renderedStmt
  else
    build (renderIndention level) renderedStmt
  where
    renderedStmt = case stmt of
      Comment comment _ ->
        build "//" comment
      Block stmts _ -> renderBlock stmts level
      Expression expr _ -> withSemicolon $ render expr
      If (expr, stmt) _ ->
        build "if (" expr ") " (renderClause stmt)
      IfElse (expr, thenStmt, elseStmt) _ ->
        build "if (" expr ") " (renderClause thenStmt)
              " else " (renderClause elseStmt)
      While (expr, stmt) _ ->
        build "while (" expr ") " (renderClause stmt)
      Global ident _ -> withSemicolon $
        build "global " ident
      Return (Just expr) _ -> withSemicolon $
        build "return " expr
      Return Nothing _ -> withSemicolon $ render "return"
      where
        withSemicolon :: Builder -> Builder
        withSemicolon builder = build builder ';'
        renderClause stmt = renderStatementIndent stmt level True

instance Renderable (PTopLevel a) where
  render toplevel = case toplevel of
    Statement stmt _ -> render stmt
    Function (name, params, stmts) _ ->
      build "function " name '(' (renderSeparateList params ", " render) ") "
            (renderBlock stmts 0)

instance Renderable (PProgram a) where
  render (Program program _) =
    build (renderSeparateList program "\n" render) '\n'

-- Below is the implementation of varidic parameter of build
class Buildable a where
  bPolyConcat :: [Builder] -> a

instance Buildable Builder where
  bPolyConcat accumulator = mconcat $ reverse accumulator

instance (Buildable a, Renderable b) => Buildable (b -> a) where
  bPolyConcat accumulator = \object ->
    let rendered = render object in
    bPolyConcat (rendered : accumulator)

build :: Buildable a => a
build = bPolyConcat []

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
