{-# LANGUAGE TupleSections, OverloadedStrings, NoMonomorphismRestriction #-}
module Run(module Parsec, module Instances, module Control.Applicative, module Control.Monad, Parser,
           go, skipWhile, string, takeWhile, takeTill, takeWhile1, char8, endOfLine) where

import Prelude hiding (takeWhile)
import Parsec
import Instances
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Test

type Parser = Parsec BS.ByteString

go :: Show a => Parser a -> String -> IO ()
go = test (\p -> run (p <* eof))

{-# INLINE skipWhile #-}
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = skip (BS.dropWhile p)

{-# INLINE string #-}
string :: BS.ByteString -> Parser BS.ByteString
string s = prim (\inp ok err->
  case BS.splitAt (BS.length s) inp of
    (xs, ys) | xs == s -> ok xs ys
    _ -> err) <* cut

{-# INLINE takeWhile1 #-}
takeWhile1 p = do
  x <- prim (\inp ok err -> case BS.uncons inp of { Nothing -> err; Just (x, _) -> ok x inp })
  guard (p x)
  takeWhile p

{-# INLINE takeTill #-}
takeTill p = takeWhile (not . p)

{-# INLINE takeWhile #-}
takeWhile p = prim (\inp ok err ->
  case BS.span p inp of
    (xs, ys) -> ok xs ys)

char8 c = satisfy (== c)

endOfLine :: Parser ()
endOfLine = (char8 '\n' >> return ()) <|> (char8 '\r' >> char8 '\n' >> return ())
