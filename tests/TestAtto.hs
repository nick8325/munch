{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}
module Main(main) where

import Control.Applicative
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as BS
import BS
import Data.Maybe

expr = chainl1 term ((+) <$ char '+' <|> (-) <$ char '-')

term = chainl1 fact ((*) <$ char '*' <|> div <$ char '/')

fact = fmap (fst . fromJust . BS.readInt) (takeWhile1 isDigit) <|> char '(' *> expr <* char ')'

eval :: BS.ByteString -> Int
eval = out . parse expr
  where out (Done _ x) = x

{-# INLINE chainl1 #-}
chainl1 :: (Monad f, Alternative f) => f a -> f (a -> a -> a) -> f a
chainl1 p op = p >>= rest where
  rest x = do f <- op
              y <- p
              rest $! (f x y)
           <|> pure x

main :: IO ()
main = do
    file <- BS.readFile "expr"
    putStr $ show $ eval file
    putStr "\n"
