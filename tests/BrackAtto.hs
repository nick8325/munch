{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}
module Main(main) where

import Control.Applicative
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as BS
import BS
import Data.Maybe

brackets = skipMany ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']'))

eval :: BS.ByteString -> ()
eval = out . parse brackets
  where out (Done _ x) = x

main :: IO ()
main = do
    file <- BS.readFile "expr"
    putStr $ show $ eval file
    putStr "\n"
