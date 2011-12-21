{-# LANGUAGE NoMonomorphismRestriction #-}
module Main(main) where

import Run

brackets = skipMany (((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']')) <?> "bracketed expression")

{-# INLINE char #-}
char :: Char -> Parser Char
char x = satisfy (== x) <?> show x

main = go brackets "testdata/brackets"