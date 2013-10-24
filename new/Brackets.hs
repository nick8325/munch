module Main where

import Test
import Control.Applicative
import Class
import Combinators

{-# INLINEABLE brackets #-}
brackets = skipMany ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']'))

main = test (readBS "../testdata/brackets") brackets