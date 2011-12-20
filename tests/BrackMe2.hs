{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, BangPatterns #-}
module BrackMe2(brackets) where

import Control.Applicative
import Control.Monad
import Parsec
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSW
import Test1

brackets = skipMany ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']'))

{-# INLINE char #-}
char :: Char -> Parsec Chars Char
char x = satisfy (== x)