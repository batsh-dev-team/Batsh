-- This file includes the parsing rules of Batsh.

{
module BatshParser(parse,
                   parseTopLevel,
                   parseStatement,
                   parseExpression) where

import qualified BatshAst
import qualified BatshLex
}

--          parse function    terminal name
%name       program           program
%name       toplevel          toplevel
%name       statement         statement
%name       expression        expression

%tokentype  { BatshLex.Token }
%error      { parseError }

%token
   ident    { BatshLex.Identifier $$ }
   comment  { BatshLex.Comment $$ }
   int      { BatshLex.Int $$ }
   float    { BatshLex.Float $$ }
   string   { BatshLex.String $$ }
   true     { BatshLex.True }
   false    { BatshLex.False }
   function { BatshLex.Function }
   global   { BatshLex.Global }
   return   { BatshLex.Return }
   '!'      { BatshLex.Not }
   ';'      { BatshLex.Semicolon }
   '+'      { BatshLex.Plus }
   '-'      { BatshLex.Minus }
   '*'      { BatshLex.Multiply }
   '/'      { BatshLex.Divide }
   '%'      { BatshLex.Modulo }
   '++'     { BatshLex.Concat }
   ','      { BatshLex.Comma }
   '('      { BatshLex.LParen }
   ')'      { BatshLex.RParen }
   '['      { BatshLex.LBrack }
   ']'      { BatshLex.RBrack }
   '{'      { BatshLex.LBrace }
   '}'      { BatshLex.RBrace }

%left '++'
%nonassoc '!'
%left '+' '-'
%left '*' '/' '%'

%%

program       : toplevels                     { $1 }

toplevel      : statement                     { BatshAst.Statement $1 }
              | function ident '(' idents ')'
                '{' statements '}'            { BatshAst.Function ($2, $4, $7)}

statement     : comment                       { BatshAst.Comment $1 }
              | '{' statements '}'            { BatshAst.Block $2 }
              | expression ';'                { BatshAst.Expression $1 }
              | global ident ';'              { BatshAst.Global $2 }
              | return expression ';'         { BatshAst.Return $ Just $2 }
              | return ';'                    { BatshAst.Return $ Nothing }

expression    : leftvalue                     { BatshAst.LeftValue $1 }
              | literal                       { BatshAst.Literal $1 }
              | ident '(' expressions ')'     { BatshAst.Call ($1, $3) }
              | '(' expression ')'            { $2 }
              | unary                         { BatshAst.Unary $1 }
              | binary                        { BatshAst.Binary $1 }

unary         : '!' expression                { BatshAst.Not, $2 }
              | '-' expression                { BatshAst.Negative, $2 }

binary        : expression '+' expression     { BatshAst.Plus, $1, $3 }
              | expression '-' expression     { BatshAst.Minus, $1, $3 }
              | expression '*' expression     { BatshAst.Multiply, $1, $3 }
              | expression '/' expression     { BatshAst.Divide, $1, $3 }
              | expression '%' expression     { BatshAst.Modulo, $1, $3 }
              | expression '++' expression    { BatshAst.Concat, $1, $3 }

literal       : true                          { BatshAst.Bool True }
              | false                         { BatshAst.Bool False }
              | int                           { BatshAst.Int $1 }
              | float                         { BatshAst.Float $1 }
              | string                        { BatshAst.String $1 }
              | '[' expressions ']'           { BatshAst.List $2 }

leftvalue     : ident                         { BatshAst.Identifier $1 }
              | leftvalue '[' expression ']'  { BatshAst.ListAccess ($1, $3) }

toplevels     :                               { [] }
              | toplevel                      { [$1] }
              | toplevel toplevels            { $1 : $2 }

statements    :                               { [] }
              | statement                     { [$1] }
              | statement statements          { $1 : $2 }

expressions   :                               { [] }
              | expression                    { [$1] }
              | expression ',' expressions    { $1 : $3 }

idents        :                               { [] }
              | ident                         { [$1] }
              | ident ',' idents              { $1 : $3 }

{
parseError :: [BatshLex.Token] -> a
parseError _ = error "Parse error"

parse :: String -> BatshAst.Program
parse = program . BatshLex.scanTokens

parseTopLevel :: String -> BatshAst.TopLevel
parseTopLevel = toplevel . BatshLex.scanTokens

parseStatement :: String -> BatshAst.Statement
parseStatement = statement . BatshLex.scanTokens

parseExpression :: String -> BatshAst.Expression
parseExpression = expression . BatshLex.scanTokens
}
