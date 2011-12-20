{-# LANGUAGE OverloadedStrings, BangPatterns, MultiParamTypeClasses, TypeFamilies, Rank2Types #-}

module MyRFC2616
    (
      Header(..)
    , Request(..)
    , Response(..)
    , isToken
    , messageHeader
    , request
    , requestLine
    , response
    , responseLine
    , lowerHeader
    , lookupHeader
    , howMany
    ) where

import Data.ByteString.Internal(c2w)
import Control.Monad
import Prelude hiding (takeWhile)
import BS
import Control.Applicative
import Parsec as P
import qualified Data.Attoparsec as AP
import qualified Data.Attoparsec.Char8 as P8
import Data.Attoparsec.Char8 (isDigit_w8)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B hiding (map)
import qualified Data.ByteString as B (map)
import qualified Data.ByteString as BS

type Parser = Parsec B.ByteString

-- data Thing = Thing { count :: {-# UNPACK #-} !Int, contents :: {-# UNPACK #-} !BS.ByteString }

-- instance Stream Thing Word8 where
--   primToken (Thing n b) ok err fatal =
--     primToken b (\b' -> ok (Thing (n+1) b')) err fatal
--   type Position Thing = BS.ByteString
--   position (Thing _ x) = x

-- type Recogniser = Parsec Thing

-- {-# INLINE recognise #-}
-- recognise :: Recogniser a -> Parser B.ByteString
-- recognise r =
--   Parsec (\ok err inp exp ->
--     runParsec r
--       (\_ _ (Thing n inp') exp' -> ok (B.take n inp) err inp' exp)
--       (\_ exp' -> err inp exp)
--       (Thing 0 inp)
--       exp)

{-# INLINE prim #-}
prim :: (forall c. a -> (b -> a -> c) -> c -> c) -> Parsec a b
prim p =
  Parsec (\ok err inp exp ->
    p inp (\x inp' -> ok x err inp' exp) (err inp exp))
-- prim p =
--   Parsec (\inp exp ok yum err ->
--     p inp (\x inp' -> yum inp' exp x) (err inp exp))
-- cut = return ()
    

endOfLine :: Parser ()
endOfLine = (word8 10 >> return ()) <|> (string "\r\n" >> return ())

skipWhile p = skipMany (satisfy p)
-- string s = Parsec (\ok err inp exp ->
--   case B.splitAt (B.length s) inp of
--     (xs, ys) | s == xs -> ok s Expected ys []
--     _ -> err inp exp)
-- FIXME why is this version slower?
-- string s = Parsec (\ok err inp exp ->
--   case B.splitAt (B.length s) inp of
--     (xs, ys) | s == xs -> ok s err ys exp
--     _ -> err inp exp) <* cut
{-# INLINE string #-}
string s =
  prim (\inp ok err ->
    case B.splitAt (B.length s) inp of
      (xs, ys) | s == xs -> ok s ys
      _ -> err) <* cut

{-# INLINE takeWhile #-}
takeWhile p = takeWhile1 p <|> return BS.empty

-- takeWhile p = Parsec (\ok err inp ->
--   case BS.span p inp of
--     (xs, inp') -> ok xs err inp')

{-# INLINE takeTill #-}
takeTill p = takeWhile (not . p)
-- takeWhile1 p = do
--   xs <- takeWhile p
--   guard (not (B.null xs))
--   return xs

{-# INLINE takeWhile1 #-}
-- takeWhile1 p = recognise (skipSome (satisfy p)) <* cut
takeWhile1 p = prim (\inp ok err ->
                           case BS.span p inp of
                                (xs, ys) | not (BS.null xs) -> ok xs ys
                                _ -> err) <* cut

char8 c = satisfy (== c2w c)
word8 c = satisfy (== c)

isToken :: Word8 -> Bool
isToken w = w <= 127 && AP.notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

{-# INLINE howMany #-}
howMany :: Parsec a b -> Parsec a Int
howMany p = p' 0 where p' !n = (nonempty p >> p' (n+1)) `mplus` return n

skipSpaces :: Parser ()
skipSpaces = satisfy P8.isHorizontalSpace *> skipWhile P8.isHorizontalSpace

data Request = Request {
      requestMethod  :: !B.ByteString
    , requestUri     :: !B.ByteString
    , requestVersion :: !B.ByteString
    } deriving (Eq, Ord, Show)

httpVersion :: Parser B.ByteString
httpVersion = string "HTTP/" *> takeWhile (\c -> isDigit_w8 c || c == 46)

requestLine :: Parser Request
requestLine = do
  method <- takeWhile1 isToken <* char8 ' '
  uri <- takeWhile1 (/=32) <* char8 ' '
  version <- httpVersion <* endOfLine
  return $! Request method uri version

data Header = Header {
      headerName  :: !B.ByteString
    , headerValue :: [B.ByteString]
    } deriving (Eq, Ord, Show)

messageHeader :: Parser Header
messageHeader = do
  header <- takeWhile isToken <* char8 ':' <* skipWhile P8.isHorizontalSpace
  body <- takeTill P8.isEndOfLine <* endOfLine
  bodies <- many $ skipSpaces *> takeTill P8.isEndOfLine <* endOfLine
  return $! Header header (body:bodies)

request :: Parser (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine

data Response = Response {
      responseVersion :: !B.ByteString
    , responseCode    :: !B.ByteString
    , responseMsg     :: !B.ByteString
    } deriving (Eq, Ord, Show)

responseLine :: Parser Response
responseLine = do
  version <- httpVersion <* char8 ' '
  code <- takeWhile isDigit_w8 <* char8 ' '
  msg <- takeTill P8.isEndOfLine <* endOfLine
  return $! Response version code msg

response :: Parser (Response, [Header])
response = (,) <$> responseLine <*> many messageHeader <* endOfLine

lowerHeader :: Header -> Header
lowerHeader (Header n v) = Header (B.map toLower n) (map (B.map toLower) v)
  where toLower w | w >= 65 && w <= 90 = w + 32
                  | otherwise          = w

lookupHeader :: B.ByteString -> [Header] -> [B.ByteString]
lookupHeader k = go
  where
    go (Header n v:hs)
      | k == n    = v
      | otherwise = go hs
    go _          = []
