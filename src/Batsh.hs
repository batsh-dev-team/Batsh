module Batsh where

import Batsh.Ast
import qualified Batsh.Parser

parseFromFile :: FilePath -> IO Program
parseFromFile path = do
  code <- readFile path
  return (Batsh.Parser.parse code)

parseFromAstFile :: FilePath -> IO Program
parseFromAstFile path = do
  code <- readFile path
  return (read code :: Program)
