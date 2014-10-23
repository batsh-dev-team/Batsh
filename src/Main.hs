import BatshLex

main = do
  s <- getContents
  print (BatshLex.alexScanTokens s)
