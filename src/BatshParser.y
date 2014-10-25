{
module BatshParser where

import qualified BatshAst
import qualified BatshLex
}

%name parseTopLevel
%tokentype { BatshLex.Token }
%error { parseError }

%token
   ident { BatshLex.Identifier $$ }
   int   { BatshLex.Int $$ }
   ','   { BatshLex.Comma }
   '['   { BatshLex.LBrack }
   ']'   { BatshLex.RBrack }
%%

program     : expression    { BatshAst.Expression $1 }

expression  : leftvalue    { BatshAst.LeftValue $1 }
            | literal      { BatshAst.Literal $1 }

literal     : int          { BatshAst.Int $1 }

leftvalue   : ident        { BatshAst.Identifier $1 }

{
parseError :: [BatshLex.Token] -> a
parseError _ = error "Parse error"

parse :: String -> BatshAst.Statement
parse = parseTopLevel . BatshLex.scanTokens
}
