module Run(module Parsec, module Instances, module Control.Applicative, module Control.Monad, Parser, go) where

import Parsec
import Instances
import Control.Applicative
import Control.Monad

type Parser = Parsec String

go :: Show a => Parser a -> String -> IO ()
go p name = do
  file <- readFile name
  print (show (run p file))
