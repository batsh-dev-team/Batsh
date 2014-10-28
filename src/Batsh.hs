module Batsh where

import BatshAst
import qualified BatshParser

parseFromFile :: FilePath -> IO Program
parseFromFile path = do
  code <- readFile path
  return (BatshParser.parse code)

parseFromAstFile :: FilePath -> IO Program
parseFromAstFile path = do
  code <- readFile path
  return (read code :: Program)
