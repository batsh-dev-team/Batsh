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
   int      { BatshLex.Int $$ }
   float    { BatshLex.Float $$ }
   string   { BatshLex.String $$ }
   true     { BatshLex.True }
   false    { BatshLex.False }
   if       { BatshLex.If }
   else     { BatshLex.Else }
   while    { BatshLex.While }
   function { BatshLex.Function }
   global   { BatshLex.Global }
   return   { BatshLex.Return }
   '!'      { BatshLex.Not }
   ';'      { BatshLex.Semicolon }
   ','      { BatshLex.Comma }
   '+'      { BatshLex.Plus }
   '-'      { BatshLex.Minus }
   '*'      { BatshLex.Multiply }
   '/'      { BatshLex.Divide }
   '%'      { BatshLex.Modulo }
   '++'     { BatshLex.Concat }
   '='      { BatshLex.Assign }
   '=='     { BatshLex.Equal }
   '!='     { BatshLex.NotEqual }
   '==='    { BatshLex.ArithEqual }
   '!=='    { BatshLex.ArithNotEqual }
   '>'      { BatshLex.Greater }
   '<'      { BatshLex.Less }
   '>='     { BatshLex.GreaterEqual }
   '<='     { BatshLex.LessEqual }
   '&&'     { BatshLex.And }
   '||'     { BatshLex.Or }
   '('      { BatshLex.LParen }
   ')'      { BatshLex.RParen }
   '['      { BatshLex.LBrack }
   ']'      { BatshLex.RBrack }
   '{'      { BatshLex.LBrace }
   '}'      { BatshLex.RBrace }
   comment  { BatshLex.Comment $$ }
   ident    { BatshLex.Identifier $$ }

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
              | if_statement                  { $1 }
              | loop_statement                { $1 }

if_statement  : if '(' expression ')'
                statement  %prec if           { BatshAst.If ($3, $5) }
              | if '(' expression ')'
                statement else statement      { BatshAst.IfElse ($3, $5, $7) }

loop_statement: while '(' expression ')'
                statement                     { BatshAst.While ($3, $5) }

expression    : leftvalue                     { BatshAst.LeftValue $1 }
              | literal                       { BatshAst.Literal $1 }
              | ident '(' expressions ')'     { BatshAst.Call ($1, $3) }
              | '(' expression ')'            { $2 }
              | unary                         { BatshAst.Unary $1 }
              | binary                        { BatshAst.Binary $1 }
              | leftvalue '=' expression      { BatshAst.Assign ($1, $3) }

unary         : '!' expression                { BatshAst.Not, $2 }
              | '-' expression %prec '-'      { BatshAst.Negate, $2 }

binary        : expression '+' expression     { BatshAst.Plus, $1, $3 }
              | expression '-' expression     { BatshAst.Minus, $1, $3 }
              | expression '*' expression     { BatshAst.Multiply, $1, $3 }
              | expression '/' expression     { BatshAst.Divide, $1, $3 }
              | expression '%' expression     { BatshAst.Modulo, $1, $3 }
              | expression '++' expression    { BatshAst.Concat, $1, $3 }
              | expression '==' expression    { BatshAst.Equal, $1, $3 }
              | expression '!=' expression    { BatshAst.NotEqual, $1, $3 }
              | expression '===' expression   { BatshAst.ArithEqual, $1, $3 }
              | expression '!==' expression   { BatshAst.ArithNotEqual, $1, $3 }
              | expression '>' expression     { BatshAst.Greater, $1, $3 }
              | expression '<' expression     { BatshAst.Less, $1, $3 }
              | expression '>=' expression    { BatshAst.GreaterEqual, $1, $3 }
              | expression '<=' expression    { BatshAst.LessEqual, $1, $3 }
              | expression '&&' expression    { BatshAst.And, $1, $3 }
              | expression '||' expression    { BatshAst.Or, $1, $3 }

literal       : true                          { BatshAst.Bool True }
              | false                         { BatshAst.Bool False }
              | int                           { BatshAst.Int $1 }
              | float                         { BatshAst.Float $1 }
              | string                        { BatshAst.String $1 }
              | '[' expressions ']'           { BatshAst.List $2 }

leftvalue     : ident                         { BatshAst.Identifier $1 }
              | leftvalue '[' expression ']'  { BatshAst.ListAccess ($1, $3) }

toplevels     :                               { [] }
              | toplevel toplevels            { $1 : $2 }

statements    :                               { [] }
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
