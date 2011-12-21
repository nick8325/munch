module Run(module Control.Applicative, module Data.Attoparsec.Char8, go) where

import Control.Applicative
import Data.Attoparsec.Char8
import qualified Data.ByteString as BS

go p name = do
  file <- BS.readFile name
  case parse p file of
    Done _ x -> return x
