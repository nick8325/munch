module Test where

import Criterion.Main
import Data.Int
import qualified Data.ByteString as BS
import System.Environment

test :: (parser -> BS.ByteString -> res) -> parser -> String -> IO ()
test parse p name = do
  prog <- getProgName
  file <- fmap (BS.take 2000000) (BS.readFile name)
  defaultMain [bench prog (whnf (parse p) file)]