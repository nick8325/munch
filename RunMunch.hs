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

type Parser = Parsec CharBS

go :: Show a => Parser a -> String -> IO ()
go = test (\p x -> run (p <* eof) (pack x))

{-# INLINE skipWhile #-}
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = skip (pack . BS.dropWhile p . unpack)

{-# INLINE string #-}
string :: BS.ByteString -> Parser BS.ByteString
string s = prim (\inp ok err->
  case BS.splitAt (BS.length s) (unpack inp) of
    (xs, ys) | xs == s -> ok xs (pack ys)
    _ -> err) <* cut

{-# INLINE takeWhile #-}
takeWhile p = takeWhile1 p <|> return BS.empty

{-# INLINE takeTill #-}
takeTill p = takeWhile (not . p)

{-# INLINE takeWhile1 #-}
takeWhile1 p = prim (\inp ok err ->
  case BS.span p (unpack inp) of
    (xs, ys) | not (BS.null xs) -> ok xs (pack ys)
    _ -> err) <* cut

char8 c = satisfy (== c)

endOfLine :: Parser ()
endOfLine = (char8 '\n' >> return ()) <|> (char8 '\r' >> char8 '\n' >> return ())
