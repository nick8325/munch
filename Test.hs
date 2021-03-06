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

test :: (Parser p, Show a) => IO (StreamType p) -> p a -> IO ()
test theFile p = do
  hSetBuffering stdout LineBuffering
  prog <- getProgName
  file <- theFile
  print (Class.run p file)
  defaultMain [bench prog (whnf (Class.run p) file)]
