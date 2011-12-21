{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}
module Main(main) where

import Run
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Char

expr = chainl1 term ((+) <$ char '+' <|> (-) <$ char '-')

term = chainl1 fact ((*) <$ char '*' <|> div <$ char '/')

fact = fmap (fst . fromJust . BS.readInt) (takeWhile1 isDigit) <|> char '(' *> expr <* char ')'

{-# INLINE chainl1 #-}
chainl1 p op = p >>= rest where
  rest x = do f <- op
              y <- p
              rest $! (f x y)
           <|> pure x

main :: IO ()
main = go expr "testdata/arith"