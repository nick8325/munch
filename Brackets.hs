{-# LANGUAGE NoMonomorphismRestriction #-}
module Main(main) where

import Run

brackets = skipMany (checkpoint *> ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']')))

main = go brackets "testdata/brackets"
