{-# LANGUAGE NoMonomorphismRestriction #-}
module Run(module Control.Applicative, module Control.Monad, module Text.Parsec, go) where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Text.Parsec
import qualified Data.ByteString.Char8 as BS
import Test

type Parser = Parsec () BS.ByteString

go = test (\p file -> parse p "" file)