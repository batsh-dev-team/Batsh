module LexerTest where

import Batsh.Lexer
import Test.HUnit

testLexer :: Assertion
testLexer = do
  let testSingle code expected = do
      let tokens = map stripPos (scanLexemes code)
      let token = head tokens
      assertEqual "Number of tokens" 1 (length tokens)
      assertEqual (show token) expected token
      where stripPos (Lex _ token) = token
  testSingle "variable" $ Identifier "variable"
  testSingle "//a line comment" $ Comment "a line comment"
  testSingle "42" $ Int 42
  testSingle "0xFF" $ Int 255
  testSingle "3.14" $ Float 3.14
  testSingle "1E-8" $ Float 1e-8
  testSingle "\"string\\n\"" $ String "string\\n"
  testSingle "true" TTrue
  testSingle "false" TFalse
  testSingle "if" If
  testSingle "else" Else
  testSingle "while" While
  testSingle "function" Function
  testSingle "global" Global
  testSingle "return" Return
  testSingle "!" Not
  testSingle ";" Semicolon
  testSingle "," Comma
  testSingle "+" Plus
  testSingle "-" Minus
  testSingle "*" Multiply
  testSingle "/" Divide
  testSingle "%" Modulo
  testSingle "++" Concat
  testSingle "=" Assign
  testSingle "==" Equal
  testSingle "!=" NotEqual
  testSingle "===" ArithEqual
  testSingle "!==" ArithNotEqual
  testSingle ">" Greater
  testSingle "<" Less
  testSingle ">=" GreaterEqual
  testSingle "<=" LessEqual
  testSingle "&&" And
  testSingle "||" Or
  testSingle "(" LParen
  testSingle ")" RParen
  testSingle "[" LBrack
  testSingle "]" RBrack
  testSingle "{" LBrace
  testSingle "}" RBrace
