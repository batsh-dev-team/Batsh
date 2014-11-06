-- This file includes the parsing rules of Batsh.

{
module Batsh.Parser(parse,
                    parseTopLevel,
                    parseStatement,
                    parseExpression) where

import qualified Batsh.Ast as Ast
import qualified Batsh.Lexer as Lexer
import qualified Batsh.Token as Token
import Prelude hiding(span)
}

--          parse function    terminal name
%name       program           program
%name       toplevel          toplevel
%name       statement         statement
%name       expression        expression

%tokentype  { Token.Lexeme }
%error      { parseError }

%token
   int      { Token.Lex _ (Token.Int _) }
   float    { Token.Lex _ (Token.Float _) }
   string   { Token.Lex _ (Token.String _) }
   true     { Token.Lex _ Token.TTrue }
   false    { Token.Lex _ Token.TFalse }
   if       { Token.Lex _ Token.If }
   else     { Token.Lex _ Token.Else }
   while    { Token.Lex _ Token.While }
   function { Token.Lex _ Token.Function }
   global   { Token.Lex _ Token.Global }
   return   { Token.Lex _ Token.Return }
   '!'      { Token.Lex _ Token.Not }
   ';'      { Token.Lex _ Token.Semicolon }
   ','      { Token.Lex _ Token.Comma }
   '+'      { Token.Lex _ Token.Plus }
   '-'      { Token.Lex _ Token.Minus }
   '*'      { Token.Lex _ Token.Multiply }
   '/'      { Token.Lex _ Token.Divide }
   '%'      { Token.Lex _ Token.Modulo }
   '++'     { Token.Lex _ Token.Concat }
   '='      { Token.Lex _ Token.Assign }
   '=='     { Token.Lex _ Token.Equal }
   '!='     { Token.Lex _ Token.NotEqual }
   '==='    { Token.Lex _ Token.ArithEqual }
   '!=='    { Token.Lex _ Token.ArithNotEqual }
   '>'      { Token.Lex _ Token.Greater }
   '<'      { Token.Lex _ Token.Less }
   '>='     { Token.Lex _ Token.GreaterEqual }
   '<='     { Token.Lex _ Token.LessEqual }
   '&&'     { Token.Lex _ Token.And }
   '||'     { Token.Lex _ Token.Or }
   '('      { Token.Lex _ Token.LParen }
   ')'      { Token.Lex _ Token.RParen }
   '['      { Token.Lex _ Token.LBrack }
   ']'      { Token.Lex _ Token.RBrack }
   '{'      { Token.Lex _ Token.LBrace }
   '}'      { Token.Lex _ Token.RBrace }
   comment  { Token.Lex _ (Token.Comment _) }
   ident    { Token.Lex _ (Token.Identifier _) }

%right if else
%right '='
%left '||'
%left '&&'
%nonassoc '==' '!=' '===' '!=='
%nonassoc '>' '<' '>=' '<='
%left '++'
%left '+' '-'
%left '*' '/' '%'
%nonassoc '!' '-'

%%

program       : toplevels                     { Ast.Program $1 $
                                                span (apos $ head $1) (apos $ last $1) }

toplevel      : statement                     { Ast.Statement $1 (apos $1) }
              | function ident '(' idents ')'
                '{' statements '}'            { Ast.Function (exStr $2, $4, $7)
                                                $ span (pos $1) (pos $8)}

statement     : comment                       { Ast.Comment (exStr $1)
                                                $ pos $1 }
              | '{' statements '}'            { Ast.Block $2
                                                $ span (pos $1) (pos $3) }
              | expression ';'                { Ast.Expression $1
                                                $ span (apos $1) (pos $2) }
              | global ident ';'              { Ast.Global (exStr $2)
                                                $ span (pos $1) (pos $3) }
              | return expression ';'         { Ast.Return (Just $2)
                                                $ span (pos $1) (pos $3) }
              | return ';'                    { Ast.Return Nothing
                                                $ span (pos $1) (pos $2) }
              | if_statement                  { $1 }
              | loop_statement                { $1 }

if_statement  : if '(' expression ')'
                statement  %prec if           { Ast.If ($3, $5)
                                                $ span (pos $1) (apos $5) }
              | if '(' expression ')'
                statement else statement      { Ast.IfElse ($3, $5, $7)
                                                $ span (pos $1) (apos $7) }

loop_statement: while '(' expression ')'
                statement                     { Ast.While ($3, $5)
                                                $ span (pos $1) (apos $5) }

expression    : leftvalue                     { Ast.LeftValue $1 (apos $1) }
              | literal                       { Ast.Literal $1 (apos $1) }
              | ident '(' expressions ')'     { Ast.Call (exStr $1, $3)
                                                $ span (pos $1) (pos $4) }
              | '(' expression ')'            { $2 }
              | unary                         { let (unary, pos) = $1 in
                                                Ast.Unary unary pos }
              | binary                        { let (binary, pos) = $1 in
                                                Ast.Binary binary pos }
              | leftvalue '=' expression      { Ast.Assign ($1, $3)
                                                $ span (apos $1) (apos $3) }

unary         : '!' expression                { (Ast.Not (pos $1), $2),
                                                span (pos $1) (apos $2) }
              | '-' expression %prec '-'      { (Ast.Negate (pos $1), $2),
                                                span (pos $1) (apos $2) }

binary        : expression '+' expression     { (Ast.Plus (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '-' expression     { (Ast.Minus (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '*' expression     { (Ast.Multiply (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '/' expression     { (Ast.Divide (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '%' expression     { (Ast.Modulo (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '++' expression    { (Ast.Concat (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '==' expression    { (Ast.Equal (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '!=' expression    { (Ast.NotEqual (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '===' expression   { (Ast.ArithEqual (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '!==' expression   { (Ast.ArithNotEqual (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '>' expression     { (Ast.Greater (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '<' expression     { (Ast.Less (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '>=' expression    { (Ast.GreaterEqual (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '<=' expression    { (Ast.LessEqual (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '&&' expression    { (Ast.And (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }
              | expression '||' expression    { (Ast.Or (pos $2), $1, $3),
                                                span (apos $1) (apos $3) }

literal       : true                          { Ast.Bool True (pos $1) }
              | false                         { Ast.Bool False (pos $1) }
              | int                           { Ast.Int (exInt $1) (pos $1) }
              | float                         { Ast.Float (exFlt $1) (pos $1) }
              | string                        { Ast.String (exStr $1) (pos $1) }
              | '[' expressions ']'           { Ast.List $2 (pos $1) }

leftvalue     : ident                         { Ast.Identifier (exStr $1) (pos $1) }
              | leftvalue '[' expression ']'  { Ast.ListAccess ($1, $3)
                                                $ span (apos $1) (pos $4) }

toplevels     :                               { [] }
              | toplevel toplevels            { $1 : $2 }

statements    :                               { [] }
              | statement statements          { $1 : $2 }

expressions   :                               { [] }
              | expression                    { [$1] }
              | expression ',' expressions    { $1 : $3 }

idents        :                               { [] }
              | ident                         { [exStr $1] }
              | ident ',' idents              { (exStr $1) : $3 }

{
-- Get the position information of a Lexeme
pos :: Token.Lexeme -> Token.LexPos
pos (Token.Lex pos _) = pos

-- Get the position information of a AstNode
apos :: Ast.AstNode a => a Token.LexPos -> Token.LexPos
apos node = Ast.annot node

-- Merge two positions from leftmost to rightmost
span :: Token.LexPos -> Token.LexPos -> Token.LexPos
span left right =
  Token.LP
    { Token.lpStartByte = start,
      Token.lpLength = length,
      Token.lpLine = Token.lpLine left,
      Token.lpColumn = Token.lpColumn left}
  where
    length = end - start
    start = Token.lpStartByte left
    end = Token.lpStartByte right + Token.lpLength right

-- Extract internal data of the Lexeme
exInt :: Token.Lexeme -> Int
exInt (Token.Lex _ (Token.Int num)) = num

exFlt :: Token.Lexeme -> Float
exFlt (Token.Lex _ (Token.Float num)) = num

exStr :: Token.Lexeme -> String
exStr (Token.Lex _ (Token.Identifier str)) = str
exStr (Token.Lex _ (Token.Comment str)) = str
exStr (Token.Lex _ (Token.String str)) = str

parseError :: [Token.Lexeme] -> a
parseError _ = error "Parse error"

parse :: String -> Ast.Program
parse code = program $ Lexer.scanLexemes code

parseTopLevel :: String -> Ast.TopLevel
parseTopLevel = toplevel . Lexer.scanLexemes

parseStatement :: String -> Ast.Statement
parseStatement = statement . Lexer.scanLexemes

parseExpression :: String -> Ast.Expression
parseExpression = expression . Lexer.scanLexemes

-- TODO add pos information to AST
scanTokens code = map stripPos (Lexer.scanLexemes code)
  where stripPos (Token.Lex _ token) = token
}
