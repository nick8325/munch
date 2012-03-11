{-# LANGUAGE NoMonomorphismRestriction #-}
module Main(main) where

import Run

brackets = skipMany (cut' $ ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']')))

main = go brackets "testdata/brackets"
