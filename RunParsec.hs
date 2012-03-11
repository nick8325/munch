{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}
module Run(module Control.Applicative, module Control.Monad, module Text.Parsec,
           Parser, go, skipWhile, string, takeWhile, takeTill, takeWhile1, char8, endOfLine, checkpoint) where

import Prelude hiding (takeWhile)
import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Text.Parsec hiding (string, chainl1, count)
import qualified Data.ByteString.Char8 as BS
import Test

type Parser = Parsec BS.ByteString ()

go = test (\p file -> parse (p <* eof) "" file)

{-# INLINE prim #-}
prim :: (forall b. BS.ByteString -> (a -> BS.ByteString -> b) -> b -> b) -> Parser a
prim f = do
  inp <- getInput
  f inp (\x inp' -> do { setInput inp'; return x }) mzero

{-# INLINE skip #-}
skip :: (BS.ByteString -> BS.ByteString) -> Parser ()
skip f = do
  inp <- getInput
  setInput (f inp)

{-# INLINE skipWhile #-}
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = skip (BS.dropWhile p)

{-# INLINE string #-}
string :: BS.ByteString -> Parser BS.ByteString
string s = prim (\inp ok err->
  case BS.splitAt (BS.length s) inp of
    (xs, ys) | xs == s -> ok xs ys
    _ -> err)

{-# INLINE takeWhile #-}
takeWhile p = prim (\inp ok err ->
  case BS.span p inp of
    (xs, ys) -> ok xs ys)

{-# INLINE takeTill #-}
takeTill p = takeWhile (not . p)

{-# INLINE takeWhile1 #-}
takeWhile1 p = do
  s <- takeWhile p
  guard (not (BS.null s))
  return s

char8 :: Char -> Parser Char
char8 c = satisfy (== c)

endOfLine :: Parser ()
endOfLine = (char8 '\n' >> return ()) <|> (char8 '\r' >> char8 '\n' >> return ())

checkpoint = return ()
