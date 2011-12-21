{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative hiding (many)
import Run as P
import Data.Attoparsec(notInClass)
import qualified Data.Attoparsec.Char8 as P8
import Data.Attoparsec.Char8 (isDigit_w8)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B hiding (map)
import qualified Data.ByteString as B (map)
import Data.ByteString.Internal(c2w)

isToken :: Word8 -> Bool
isToken w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

skipSpaces :: Parser ()
skipSpaces = satisfy (P8.isHorizontalSpace . c2w) *> skipWhile (P8.isHorizontalSpace . c2w)

data Request = Request {
      requestMethod  :: !B.ByteString
    , requestUri     :: !B.ByteString
    , requestVersion :: !B.ByteString
    } deriving (Eq, Ord, Show)

httpVersion :: Parser B.ByteString
httpVersion = string "HTTP/" *> P.takeWhile (\c -> isDigit_w8 (c2w c) || c == toEnum 46)

requestLine :: Parser Request
requestLine = do
  method <- P.takeWhile1 (isToken . c2w) <* char8 ' '
  uri <- P.takeWhile1 (/= ' ') <* char8 ' '
  version <- httpVersion <* endOfLine
  return $! Request method uri version

data Header = Header {
      headerName  :: !B.ByteString
    , headerValue :: [B.ByteString]
    } deriving (Eq, Ord, Show)

messageHeader :: Parser Header
messageHeader = checkpoint *> do
  header <- P.takeWhile (isToken . c2w) <* char8 ':' <* skipWhile (P8.isHorizontalSpace . c2w)
  body <- takeTill (P8.isEndOfLine . c2w) <* endOfLine
  bodies <- many $ skipSpaces *> takeTill (P8.isEndOfLine . c2w) <* endOfLine
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
  code <- P.takeWhile (isDigit_w8 . c2w) <* char8 ' '
  msg <- P.takeTill (P8.isEndOfLine . c2w) <* endOfLine
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

main = go (skipMany request) "testdata/http"