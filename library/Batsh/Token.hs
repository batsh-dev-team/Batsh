module Batsh.Token where

data Token
  = Identifier String
  | Comment String
  | Int Int
  | Float Float
  | String String
  | TTrue
  | TFalse
  | If
  | Else
  | While
  | Function
  | Global
  | Return
  | Not
  | Semicolon
  | Comma
  | Plus
  | Minus
  | Multiply
  | Divide
  | Modulo
  | Concat
  | Assign
  | Equal
  | NotEqual
  | ArithEqual
  | ArithNotEqual
  | Greater
  | Less
  | GreaterEqual
  | LessEqual
  | And
  | Or
  | LParen
  | RParen
  | LBrack
  | RBrack
  | LBrace
  | RBrace
  | LEOF
  deriving (Eq, Read, Show)

data LexPos = LP
  { lpLine :: Int,
    lpColumn :: Int,
    lpStartByte :: Int,
    lpLength :: Int }
  deriving (Eq, Read, Show)

data Lexeme = Lex LexPos Token deriving (Eq, Read, Show)
