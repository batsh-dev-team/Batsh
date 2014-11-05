-- This file includes the parsing rules of Batsh.

{
module Batsh.Parser(parse,
                    parseTopLevel,
                    parseStatement,
                    parseExpression) where

import qualified Batsh.Ast as Ast
import qualified Batsh.Lexer as Lexer
}

--          parse function    terminal name
%name       program           program
%name       toplevel          toplevel
%name       statement         statement
%name       expression        expression

%tokentype  { Lexer.Token }
%error      { parseError }

%token
   int      { Lexer.Int $$ }
   float    { Lexer.Float $$ }
   string   { Lexer.String $$ }
   true     { Lexer.TTrue }
   false    { Lexer.TFalse }
   if       { Lexer.If }
   else     { Lexer.Else }
   while    { Lexer.While }
   function { Lexer.Function }
   global   { Lexer.Global }
   return   { Lexer.Return }
   '!'      { Lexer.Not }
   ';'      { Lexer.Semicolon }
   ','      { Lexer.Comma }
   '+'      { Lexer.Plus }
   '-'      { Lexer.Minus }
   '*'      { Lexer.Multiply }
   '/'      { Lexer.Divide }
   '%'      { Lexer.Modulo }
   '++'     { Lexer.Concat }
   '='      { Lexer.Assign }
   '=='     { Lexer.Equal }
   '!='     { Lexer.NotEqual }
   '==='    { Lexer.ArithEqual }
   '!=='    { Lexer.ArithNotEqual }
   '>'      { Lexer.Greater }
   '<'      { Lexer.Less }
   '>='     { Lexer.GreaterEqual }
   '<='     { Lexer.LessEqual }
   '&&'     { Lexer.And }
   '||'     { Lexer.Or }
   '('      { Lexer.LParen }
   ')'      { Lexer.RParen }
   '['      { Lexer.LBrack }
   ']'      { Lexer.RBrack }
   '{'      { Lexer.LBrace }
   '}'      { Lexer.RBrace }
   comment  { Lexer.Comment $$ }
   ident    { Lexer.Identifier $$ }

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

toplevel      : statement                     { Ast.Statement $1 }
              | function ident '(' idents ')'
                '{' statements '}'            { Ast.Function ($2, $4, $7)}

statement     : comment                       { Ast.Comment $1 }
              | '{' statements '}'            { Ast.Block $2 }
              | expression ';'                { Ast.Expression $1 }
              | global ident ';'              { Ast.Global $2 }
              | return expression ';'         { Ast.Return $ Just $2 }
              | return ';'                    { Ast.Return $ Nothing }
              | if_statement                  { $1 }
              | loop_statement                { $1 }

if_statement  : if '(' expression ')'
                statement  %prec if           { Ast.If ($3, $5) }
              | if '(' expression ')'
                statement else statement      { Ast.IfElse ($3, $5, $7) }

loop_statement: while '(' expression ')'
                statement                     { Ast.While ($3, $5) }

expression    : leftvalue                     { Ast.LeftValue $1 }
              | literal                       { Ast.Literal $1 }
              | ident '(' expressions ')'     { Ast.Call ($1, $3) }
              | '(' expression ')'            { $2 }
              | unary                         { Ast.Unary $1 }
              | binary                        { Ast.Binary $1 }
              | leftvalue '=' expression      { Ast.Assign ($1, $3) }

unary         : '!' expression                { Ast.Not, $2 }
              | '-' expression %prec '-'      { Ast.Negate, $2 }

binary        : expression '+' expression     { Ast.Plus, $1, $3 }
              | expression '-' expression     { Ast.Minus, $1, $3 }
              | expression '*' expression     { Ast.Multiply, $1, $3 }
              | expression '/' expression     { Ast.Divide, $1, $3 }
              | expression '%' expression     { Ast.Modulo, $1, $3 }
              | expression '++' expression    { Ast.Concat, $1, $3 }
              | expression '==' expression    { Ast.Equal, $1, $3 }
              | expression '!=' expression    { Ast.NotEqual, $1, $3 }
              | expression '===' expression   { Ast.ArithEqual, $1, $3 }
              | expression '!==' expression   { Ast.ArithNotEqual, $1, $3 }
              | expression '>' expression     { Ast.Greater, $1, $3 }
              | expression '<' expression     { Ast.Less, $1, $3 }
              | expression '>=' expression    { Ast.GreaterEqual, $1, $3 }
              | expression '<=' expression    { Ast.LessEqual, $1, $3 }
              | expression '&&' expression    { Ast.And, $1, $3 }
              | expression '||' expression    { Ast.Or, $1, $3 }

literal       : true                          { Ast.Bool True }
              | false                         { Ast.Bool False }
              | int                           { Ast.Int $1 }
              | float                         { Ast.Float $1 }
              | string                        { Ast.String $1 }
              | '[' expressions ']'           { Ast.List $2 }

leftvalue     : ident                         { Ast.Identifier $1 }
              | leftvalue '[' expression ']'  { Ast.ListAccess ($1, $3) }

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
parseError :: [Lexer.Token] -> a
parseError _ = error "Parse error"

parse :: String -> Ast.Program
parse code = Ast.Program $ program $ scanTokens code

parseTopLevel :: String -> Ast.TopLevel
parseTopLevel = toplevel . scanTokens

parseStatement :: String -> Ast.Statement
parseStatement = statement . scanTokens

parseExpression :: String -> Ast.Expression
parseExpression = expression . scanTokens

-- TODO add pos information to AST
scanTokens code = map stripPos (Lexer.scanLexemes code)
  where stripPos (Lexer.Lex _ token) = token
}
