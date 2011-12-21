{-# LANGUAGE NoMonomorphismRestriction #-}
module Main(main) where

import Run

brackets = checkpoint *> skipMany (((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']')) <?> "bracketed expression")

main = go brackets "testdata/brackets"