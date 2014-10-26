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
   ','      { BatshLex.Comma }
   '['      { BatshLex.LBrack }
   ']'      { BatshLex.RBrack }
%%

program     : toplevels           { $1 }

toplevels   :                     { [] }
            | toplevel            { [$1] }
            | toplevel toplevels  { $1 : $2 }

toplevel    : statement           { BatshAst.Statement $1 }

statement   : comment             { BatshAst.Comment $1 }
            | expression          { BatshAst.Expression $1 }

expression  : leftvalue           { BatshAst.LeftValue $1 }
            | literal             { BatshAst.Literal $1 }

literal     : true                { BatshAst.Bool True }
            | false               { BatshAst.Bool False }
            | int                 { BatshAst.Int $1 }
            | float               { BatshAst.Float $1 }
            | string              { BatshAst.String $1 }
            | '[' list ']'        { BatshAst.List $2 }

list        :                     { [] }
            | expression          { [$1] }
            | expression ',' list { $1 : $3 }

leftvalue   : ident               { BatshAst.Identifier $1 }

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
