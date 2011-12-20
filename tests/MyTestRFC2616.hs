{-# LANGUAGE BangPatterns #-}
import MyRFC2616
import Control.Monad (forM_)
import System.IO
import Control.Exception (bracket)
import System.Environment
import qualified Data.ByteString.Char8 as B
import Parsec
import BS

main = do
  args <- getArgs
  forM_ args $ \arg -> do
    c <- B.readFile arg
    print (snd (run (const []) (howMany request) c))