-- This file is the lexical syntax of Batsh.
-- Rules are inspired by Haskell alex examples/haskell.x

{
module Batsh.Lexer where
}

%wrapper "monad"

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
  $white+     { skip }
  @decimal    { makeReadableLexeme Int }
  @hexadecimal{ makeReadableLexeme Int }
  @float      { makeReadableLexeme Float }
  @string     { makeStringLexeme $ \s -> String (tail $ init s) }
  "true"      { makeLexeme TTrue }
  "false"     { makeLexeme TFalse }
  "if"        { makeLexeme If }
  "else"      { makeLexeme Else }
  "while"     { makeLexeme While }
  "function"  { makeLexeme Function }
  "global"    { makeLexeme Global }
  "return"    { makeLexeme Return }
  "!"         { makeLexeme Not }
  ";"         { makeLexeme Semicolon }
  ","         { makeLexeme Comma }
  "+"         { makeLexeme Plus }
  "-"         { makeLexeme Minus }
  "*"         { makeLexeme Multiply }
  "/"         { makeLexeme Divide }
  "%"         { makeLexeme Modulo }
  "++"        { makeLexeme Concat }
  "="         { makeLexeme Assign }
  "=="        { makeLexeme Equal }
  "!="        { makeLexeme NotEqual }
  "==="       { makeLexeme ArithEqual }
  "!=="       { makeLexeme ArithNotEqual }
  ">"         { makeLexeme Greater }
  "<"         { makeLexeme Less }
  ">="        { makeLexeme GreaterEqual }
  "<="        { makeLexeme LessEqual }
  "&&"        { makeLexeme And }
  "||"        { makeLexeme Or }
  "("         { makeLexeme LParen }
  ")"         { makeLexeme RParen }
  "["         { makeLexeme LBrack }
  "]"         { makeLexeme RBrack }
  "{"         { makeLexeme LBrace }
  "}"         { makeLexeme RBrace }
  "//".*      { makeStringLexeme $ \s -> Comment $ drop 2 s }
  @identifier { makeStringLexeme Identifier }

{

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

makeLexPos :: AlexPosn -> Int -> LexPos
makeLexPos (AlexPn startByte line column) length =
  LP {lpStartByte = startByte,
      lpLength = length,
      lpLine = line,
      lpColumn = column}

getMatchedString :: AlexInput -> Int -> String
getMatchedString (_, _, _, str) len = take len str

makeStringLexeme :: (String -> Token) -> AlexInput -> Int -> Alex Lexeme
makeStringLexeme cons input len =
  return $ Lex (makeLexPos pos len) token
  where
    (pos, _, _, _) = input
    token = cons $ getMatchedString input len

makeReadableLexeme :: (Read a) =>
                      (a -> Token) -> AlexInput -> Int -> Alex Lexeme
makeReadableLexeme cons input len =
  return $ Lex (makeLexPos pos len) token
  where
    (pos, _, _, _) = input
    token = cons $ read (getMatchedString input len)

makeLexeme :: Token -> AlexInput -> Int -> Alex Lexeme
makeLexeme token (pos, _, _, _) len = return $ Lex (makeLexPos pos len) token

alexEOF :: Alex Lexeme
alexEOF = return (Lex undefined LEOF)

-- Returns scanned lexemes with Right [Token] or Left String on error
scanLexemesSafe :: String -> Either String [Lexeme]
scanLexemesSafe code = runAlex code $ do
  let loop i lexemes = do
      lexeme@(Lex _ token) <- alexMonadScan;
        if token == LEOF then
          return $ reverse lexemes
        else do
          loop (i + 1) (lexeme : lexemes)
  loop 0 []

-- Returns scanned lexemes
scanLexemes :: String -> [Lexeme]
scanLexemes code = case scanLexemesSafe code of
  Left message -> error message
  Right lexemes -> lexemes

}
