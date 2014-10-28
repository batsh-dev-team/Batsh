import qualified Batsh.Parser
import Text.Show.Pretty(ppShow)

main = do
  s <- getContents
  putStrLn (ppShow $ Batsh.Parser.parse s)
