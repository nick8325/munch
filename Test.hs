module Test where

import Criterion.Main
import Data.Int
import qualified Data.ByteString as BS
import System.Environment
import System.IO

test :: Show res => (parser -> BS.ByteString -> res) -> parser -> String -> IO ()
test parse p name = do
  hSetBuffering stdout LineBuffering
  prog <- getProgName
  file <- fmap (BS.take 2000000) (BS.readFile name)
  print (parse p file)
  defaultMain [bench prog (whnf (parse p) file)]