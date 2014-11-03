{-# LANGUAGE Arrows #-}

import Control.Monad
import Options.Applicative
import Options.Applicative.Arrows
import Text.Show.Pretty(ppShow)

import qualified Batsh

data Args = Args CommonOpts Command
  deriving Show

data CommonOpts = CommonOpts
  { optVerbosity :: Int }
  deriving Show

data Command
  = Batsh FilePath FilePath BatshOpts
  | Bash
  | Winbat
  deriving Show

data BatshOpts = BatshOpts
  { batshOptsAst :: Bool,
    batshOptsTokens :: Bool,
    batshOptsSymbols :: Bool }
  deriving Show

version :: Parser (a -> a)
version = infoOption "0.1.0"
  (  long "version"
  <> short 'v'
  <> help "Print version information" )

parser :: Parser Args
parser = runA $ proc () -> do
  opts <- asA commonOpts -< ()
  cmds <- (asA . hsubparser)
            ( command "batsh"
              (info batshParser
                    (progDesc "Format Batsh code"))
           <> command "bash"
              (info bashParser
                    (progDesc "Compile to Bash"))
           <> command "winbat"
              (info winbatParser
                    (progDesc "Compile to Windows Batch"))
            ) -< ()
  A version >>> A helper -< Args opts cmds

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts
  <$> option auto
      ( long "verbose"
     <> metavar "LEVEL"
     <> help "Set verbosity to LEVEL"
     <> value 0 )

batshParser :: Parser Command
batshParser = runA $ proc () -> do
  input <- asA (strArgument (metavar "INPUT"
      <> help "Source code file")) -< ()
  output <- asA (strArgument (metavar "TARGET"
      <> help "Target file")) -< ()
  opts <- asA batshOpts -< ()
  returnA -< Batsh input output opts

batshOpts :: Parser BatshOpts
batshOpts = runA $ proc () -> do
  ast <- asA (switch (long "ast"
      <> help "Output parsed abstract syntax tree")) -< ()
  tokens <- asA (switch (long "tokens"
      <> help "Output parsed tokens")) -< ()
  symbols <- asA (switch (long "symbols"
      <> help "Output symbol table")) -< ()
  returnA -< BatshOpts {
    batshOptsAst = ast,
    batshOptsTokens = tokens,
    batshOptsSymbols = symbols}

bashParser :: Parser Command
bashParser = pure Bash

winbatParser :: Parser Command
winbatParser = pure Winbat

dispatch :: Args -> IO ()
dispatch (Args opts cmd) = case cmd of
  Batsh input target opts -> batsh input target opts

batsh :: FilePath -> FilePath -> BatshOpts -> IO ()
batsh input target opts = do
  code <- readFile input
  let program = Batsh.parse code
  let tokens = Batsh.lex code
  let symbols = Batsh.createSymbolTable program
  let outputWithSuffix :: String -> String -> IO ();
      outputWithSuffix suffix contents = do
        let fileName = target ++ suffix
        writeFile fileName (contents ++ "\n")
  when (batshOptsTokens opts) (outputWithSuffix ".tokens" (ppShow tokens))
  when (batshOptsAst opts) (outputWithSuffix ".ast" (ppShow program))
  when (batshOptsSymbols opts) (outputWithSuffix ".symbols" (ppShow symbols))
  Batsh.generateCodeToFile program target

pinfo :: ParserInfo Args
pinfo = info parser $
  progDesc "A language that compiles to Bash and Windows Batch"

main :: IO ()
main = do
  args <- execParser pinfo
  dispatch args
