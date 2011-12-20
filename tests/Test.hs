module Test where

import BS
import Parsec
import Control.Applicative
import qualified Data.ByteString as B

string1 s = Parsec (\ok err inp exp ->
  case B.splitAt (B.length s) inp of
    (xs, ys) | s == xs -> ok s Expected ys []
    _ -> err inp exp)

string2 s = Parsec (\ok err inp ->
  case B.splitAt (B.length s) inp of
    (xs, ys) | s == xs -> ok s err ys
    _ -> err inp) <* cut