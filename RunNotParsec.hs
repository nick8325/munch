module Run(module Parsec, module Instances, module Control.Applicative, module Control.Monad, Parser, go) where

import Parsec
import Instances
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Test

type Parser = Parsec BS.ByteString

go :: Show a => Parser a -> String -> IO ()
go = test BS.readFile run