import qualified BatshLex

main = do
  s <- getContents
  print (BatshLex.scanTokens s)
