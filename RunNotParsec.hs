module Run(module Parsec, module Instances, module Control.Applicative, module Control.Monad, Parser, go) where

import Parsec
import Instances
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS

type Parser = Parsec BS.ByteString

go :: Show a => Parser a -> String -> IO ()
go p name = do
  file <- BS.readFile name
  print (show (run p file))
