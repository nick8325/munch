{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, BangPatterns #-}
module Main(main) where

import Control.Applicative
import Control.Monad
import Parsec
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSW
import BS
import Test1
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- type X = CharBS
-- conv = charBS

-- type X = BS.ByteString
-- conv = id

type X = T.Text
conv = id

-- type X = Chars
-- conv = fromStr . BS.unpack

-- brackets = skipMany (((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']')) <?> "bracketed expression")

brackets = skipMany (char '(')

{-# INLINE char #-}
char :: Char -> Parsec X Char
char x = satisfy (== x) <?> show x

parse p x = run (const []) p
eval :: X -> ()
eval = either (error . show) id . snd . parse brackets ""

main :: IO ()
main = do
    file <- T.readFile "expr"
    putStr $ show $ eval (conv file)
    putStr "\n"
