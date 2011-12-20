import Control.Applicative
import Control.Monad
import Text.Parsec hiding ((<|>))
import qualified Data.ByteString.Lazy.Char8 as BS

expr = chainl1 term ((+) <$ char '+' <|> (-) <$ char '-')

term = chainl1 fact ((*) <$ char '*' <|> div <$ char '/')

fact = readNum <|> char '(' *> expr <* char ')'
readNum = do
  i <- getInput
  case BS.readInt i of
    Just (n, i') -> do
      setInput i'
      return n
    Nothing -> mzero

eval :: BS.ByteString -> Int
eval = either (error . show) id . parse expr ""

main :: IO ()
main = do
    file <- BS.readFile "expr"
    putStr $ show $ eval file
    putStr "\n"
