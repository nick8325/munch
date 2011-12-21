{-# LANGUAGE NoMonomorphismRestriction #-}
module Run(module Control.Applicative, module Data.Attoparsec.Char8, go) where

import Control.Applicative
import Data.Attoparsec.Char8
import qualified Data.ByteString as BS
import Test

go = test BS.readFile f
  where
    f p file = case parse p file of
      Done _ x -> x
