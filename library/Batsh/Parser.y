-- This file includes the parsing rules of Batsh.

{
module Batsh.Parser(parse,
                    parseTopLevel,
                    parseStatement,
                    parseExpression) where

import qualified Batsh.Ast as Ast
import qualified Batsh.Lexer as Lexer
import Prelude hiding(span)
}

--          parse function    terminal name
%name       program           program
%name       toplevel          toplevel
%name       statement         statement
%name       expression        expression

%tokentype  { Lexer.Lexeme }
%error      { parseError }

%token
   int      { Lexer.Lex _ (Lexer.Int _) }
   float    { Lexer.Lex _ (Lexer.Float _) }
   string   { Lexer.Lex _ (Lexer.String _) }
   true     { Lexer.Lex _ Lexer.TTrue }
   false    { Lexer.Lex _ Lexer.TFalse }
   if       { Lexer.Lex _ Lexer.If }
   else     { Lexer.Lex _ Lexer.Else }
   while    { Lexer.Lex _ Lexer.While }
   function { Lexer.Lex _ Lexer.Function }
   global   { Lexer.Lex _ Lexer.Global }
   return   { Lexer.Lex _ Lexer.Return }
   '!'      { Lexer.Lex _ Lexer.Not }
   ';'      { Lexer.Lex _ Lexer.Semicolon }
   ','      { Lexer.Lex _ Lexer.Comma }
   '+'      { Lexer.Lex _ Lexer.Plus }
   '-'      { Lexer.Lex _ Lexer.Minus }
   '*'      { Lexer.Lex _ Lexer.Multiply }
   '/'      { Lexer.Lex _ Lexer.Divide }
   '%'      { Lexer.Lex _ Lexer.Modulo }
   '++'     { Lexer.Lex _ Lexer.Concat }
   '='      { Lexer.Lex _ Lexer.Assign }
   '=='     { Lexer.Lex _ Lexer.Equal }
   '!='     { Lexer.Lex _ Lexer.NotEqual }
   '==='    { Lexer.Lex _ Lexer.ArithEqual }
   '!=='    { Lexer.Lex _ Lexer.ArithNotEqual }
   '>'      { Lexer.Lex _ Lexer.Greater }
   '<'      { Lexer.Lex _ Lexer.Less }
   '>='     { Lexer.Lex _ Lexer.GreaterEqual }
   '<='     { Lexer.Lex _ Lexer.LessEqual }
   '&&'     { Lexer.Lex _ Lexer.And }
   '||'     { Lexer.Lex _ Lexer.Or }
   '('      { Lexer.Lex _ Lexer.LParen }
   ')'      { Lexer.Lex _ Lexer.RParen }
   '['      { Lexer.Lex _ Lexer.LBrack }
   ']'      { Lexer.Lex _ Lexer.RBrack }
   '{'      { Lexer.Lex _ Lexer.LBrace }
   '}'      { Lexer.Lex _ Lexer.RBrace }
   comment  { Lexer.Lex _ (Lexer.Comment _) }
   ident    { Lexer.Lex _ (Lexer.Identifier _) }

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
pos :: Lexer.Lexeme -> Lexer.LexPos
pos (Lexer.Lex pos _) = pos

-- Get the position information of a AstNode
apos :: Ast.AstNode a => a Lexer.LexPos -> Lexer.LexPos
apos node = Ast.annot node

-- Merge two positions from leftmost to rightmost
span :: Lexer.LexPos -> Lexer.LexPos -> Lexer.LexPos
span left right =
  Lexer.LP
    { Lexer.lpStartByte = start,
      Lexer.lpLength = length,
      Lexer.lpLine = Lexer.lpLine left,
      Lexer.lpColumn = Lexer.lpColumn left}
  where
    length = end - start
    start = Lexer.lpStartByte left
    end = Lexer.lpStartByte right + Lexer.lpLength right

-- Extract internal data of the Lexeme
exInt :: Lexer.Lexeme -> Int
exInt (Lexer.Lex _ (Lexer.Int num)) = num

exFlt :: Lexer.Lexeme -> Float
exFlt (Lexer.Lex _ (Lexer.Float num)) = num

exStr :: Lexer.Lexeme -> String
exStr (Lexer.Lex _ (Lexer.Identifier str)) = str
exStr (Lexer.Lex _ (Lexer.Comment str)) = str
exStr (Lexer.Lex _ (Lexer.String str)) = str

parseError :: [Lexer.Lexeme] -> a
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
  where stripPos (Lexer.Lex _ token) = token
}
