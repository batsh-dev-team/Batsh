{
module BatshLex where
}

%wrapper "basic"

$digit = [0-9]
$oct_digit = [0-7]
$hex_digit = [0-9A-Fa-f]
$alpha = [a-zA-Z]
$underscore = \_

@integer = $digit+
@frac = \. $digit*
@exp = [eE][\-\+]? $digit+
@float = $digit* @frac @exp? | $digit* @exp
@newline = '\r' | '\n' | "\r\n"
@identifier = [$alpha $underscore] [$alpha $digit $underscore]*

tokens :-
  $white+ ;
  "//".* ;
  @integer { \s -> Int (read s) }
  @float { \s -> Float (read s) }
  "true" { \s -> BatshLex.True }
  "false" { \s -> BatshLex.False }
  @identifier { \s -> Identifier s }
  [\=\+\-\*\/\(\)] { \s -> Sym (head s) }

{
data Token = Identifier String
  | Sym Char
  | Int Int
  | Float Float
  | True
  | False
  | If
  | Else
  | While
  | Function
  | Global
  | Return
  deriving (Eq,Show)
}
