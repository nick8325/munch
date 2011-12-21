module Test where

import Criterion.Main
import Data.Int
import qualified Data.ByteString as BS
import System.Environment
import System.IO

test :: (parser -> BS.ByteString -> res) -> parser -> String -> IO ()
test parse p name = do
  hSetBuffering stdout LineBuffering
  prog <- getProgName
  file <- fmap (BS.take 2000000) (BS.readFile name)
  defaultMain [bench prog (whnf (parse p) file)]