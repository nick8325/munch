module Test where

import Criterion.Main
import qualified Data.ByteString as B
import System.Environment
import System.IO
import Control.Monad
import Look
import Simple
import Stream
import Class

readBS :: String -> IO B.ByteString
readBS name = fmap (B.take 2000000) (B.readFile name)

test :: (Stream s, Show a) => IO s -> Look (Simple s) a -> IO ()
test theFile p = do
  hSetBuffering stdout LineBuffering
  prog <- getProgName
  file <- theFile
  replicateM_ 100 (print (Class.run p file))
  -- defaultMain [bench prog (whnf (parse p) file)]
