import Control.Applicative
import Control.Monad
import Text.Parsec hiding ((<|>))
import qualified Data.ByteString.Lazy.Char8 as BS

brackets = skipMany ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']'))

eval :: BS.ByteString -> ()
eval = either (error . show) id . parse brackets ""

main :: IO ()
main = do
    file <- BS.readFile "expr"
    putStr $ show $ eval file
    putStr "\n"
