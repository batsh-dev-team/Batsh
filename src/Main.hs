import qualified BatshParser
import Text.Show.Pretty(ppShow)

main = do
  s <- getContents
  putStrLn (ppShow $ BatshParser.parse s)
