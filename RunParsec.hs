module Run(module Control.Applicative, module Control.Monad, module Text.Parsec, go) where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Text.Parsec
import qualified Data.ByteString.Char8 as BS

type Parser = Parsec () BS.ByteString

go p name = do
  file <- BS.readFile name
  print (parse p "" file)