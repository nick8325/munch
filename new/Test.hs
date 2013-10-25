module Test where

import Criterion.Main
import qualified Data.ByteString as B
import System.Environment
import System.IO
import Control.Monad
import Stream
import Class

readBS :: String -> IO B.ByteString
readBS name = fmap (B.take 2000000) (B.readFile name)

{-# NOINLINE go #-}
go p file = return (Class.run p file) >>= print

test :: (Parser p, Show a) => IO (StreamType p) -> p a -> IO ()
test theFile p = do
  hSetBuffering stdout LineBuffering
  prog <- getProgName
  file <- theFile
  replicateM_ 100 (go p file)
  -- defaultMain [bench prog (whnf (parse p) file)]
