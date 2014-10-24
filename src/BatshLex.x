-- This file is the lexical syntax of Batsh.
-- Rules are inspired by Haskell alex examples/haskell.x

{
module BatshLex where
}

%wrapper "basic"

$underscore = \_
$whitechar =  [ \t\n\r\f\v]
$newline =    [\r\n]
$digit =      [0-9]
$oct_digit =  [0-7]
$hex_digit =  [0-9A-Fa-f]
$large =      [A-Z \xc0-\xd6 \xd8-\xde]
$small =      [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha =      [$small $large]

@identifier = [$alpha $underscore] [$alpha $digit $underscore]*

-- Integers
@decimal =    $digit+
@hexadecimal = 0x $hex_digit+
@octal =      0 $oct_digit+

-- Float
@frac =       \. $digit*
@exp =        [eE][\-\+]? $digit+
@float =      $digit* @frac @exp? | $digit* @exp

-- String
$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
   | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
   | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
   | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | @octal | @hexadecimal)
@gap     = \\ $whitechar+ \\
@string_in  = . # [\"\\] | " " | @escape | @gap
@string  = \" @string_in* \"

tokens :-
  $white+     ;
  @decimal    { \s -> Int (read s) }
  @hexadecimal{ \s -> Int (read s) }
  @float      { \s -> Float (read s) }
  @string     { \s -> String (tail $ init s) }
  "true"      { \s -> BatshLex.True }
  "false"     { \s -> BatshLex.False }
  "if"        { \s -> If }
  "else"      { \s -> Else }
  "while"     { \s -> While }
  "function"  { \s -> Function }
  "global"    { \s -> Global }
  "return"    { \s -> Return }
  "!"         { \s -> Not }
  ";"         { \s -> Semicolon }
  ","         { \s -> Comma }
  "+"         { \s -> Plus }
  "-"         { \s -> Minus }
  "*"         { \s -> Multiply }
  "/"         { \s -> Divide }
  "%"         { \s -> Modulo }
  "++"        { \s -> Concat }
  "="         { \s -> Assign }
  "=="        { \s -> Equal }
  "!="        { \s -> NotEqual }
  "==="       { \s -> ArithEqual }
  "!=="       { \s -> ArithNotEqual }
  ">"         { \s -> Greater }
  "<"         { \s -> Less }
  ">="        { \s -> GreaterEqual }
  "<="        { \s -> LessEqual }
  "("         { \s -> LParen }
  ")"         { \s -> RParen }
  "["         { \s -> LBrack }
  "]"         { \s -> RBrack }
  "{"         { \s -> LBrace }
  "}"         { \s -> RBrace }
  "//".*      { \s -> Comment $ drop 2 s }
  @identifier { \s -> Identifier s }

{
data Token = Identifier String
  | Comment String
  | Int Int
  | Float Float
  | String String
  | True
  | False
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
  | LParen
  | RParen
  | LBrack
  | RBrack
  | LBrace
  | RBrace
  deriving (Eq,Show)
}
