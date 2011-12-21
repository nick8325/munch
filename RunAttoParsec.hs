{-# LANGUAGE NoMonomorphismRestriction #-}
module Run(module Control.Applicative, module Data.Attoparsec.Char8, go) where

import Control.Applicative
import Data.Attoparsec.Char8 hiding (notInClass)
import qualified Data.ByteString.Char8 as BS
import Test

go = test f
  where
    f p file = decode (parse p file)
    decode (Done _ x) = Right x
    decode (Fail _ xs x) = Left (xs, x)
    decode (Partial f) = decode (f BS.empty)
