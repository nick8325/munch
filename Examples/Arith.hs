module Examples.Arith where

import Control.Applicative
import Class
import Combinators
import Simple
import Look
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Char

expr, term, fact :: Look (Simple B.ByteString) ()
expr = chainl1 term ((+) <$ char '+' <|> (-) <$ char '-')
term = chainl1 fact ((*) <$ char '*' <|> div <$ char '/')
fact = fmap (fst . fromJust . B.readInt) (takeWhile1 isDigit) <|> char '(' *> expr <* char ')'

{-# INLINE chainl1 #-}
chainl1 p op = p >>= rest where
  rest x = do f <- op
              y <- p
              rest $! (f x y)
           <|> pure x
