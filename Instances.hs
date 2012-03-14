{-# LANGUAGE TypeFamilies, BangPatterns, MagicHash #-}
module Instances where

import Parsec
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSW
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Internal as BSI
import Data.Word
import Foreign.Storable
import Foreign.ForeignPtr
import qualified Data.Text as T
import Data.Char
import GHC.Base(unsafeChr)
import GHC.Prim
import GHC.Ptr
import GHC.ForeignPtr
import GHC.Types

instance Stream BS.ByteString where
  type Token BS.ByteString = Char
  {-# INLINE primToken #-}
  primToken bs ok err _ =
    case BS.uncons bs of
      Nothing -> err
      Just (c, cs) -> ok cs c
  {-# INLINE hd #-}
  hd x =
    case BS.uncons x of
      Nothing -> '\000'
      Just (c, _) -> c
  append = BS.append
  pos = BS.length

instance Stream T.Text where
  type Token T.Text = Char
  {-# INLINE primToken #-}
  primToken t ok err _ =
    case T.uncons t of
      Nothing -> err
      Just (c, cs) -> ok cs c

data Chars = Cons {-# UNPACK #-} !Char Chars

instance Stream Chars where
  type Token Chars = Char
  {-# INLINE primToken #-}
  primToken (Cons c cs) ok _ _ = ok cs c

instance Stream [a] where
  type Token [a] = a
  {-# INLINE primToken #-}
  primToken [] _ err _ = err
  primToken (x:xs) ok _ _ = ok xs x

data CharBS = CharBS Char {-# UNPACK #-} !BS.ByteString

pack bs = CharBS (nextChar bs) bs

instance Stream CharBS where
  type Token CharBS = Char
  
  primToken (CharBS !c s) ok err _
    | BS.null s = err
    | otherwise =
        (ok $! CharBS (nextChar (BSU.unsafeTail s)) (BSU.unsafeTail s)) c

{-# INLINE nextChar #-}
nextChar :: BS.ByteString -> Char
nextChar bs =
  case BSI.toForeignPtr bs of
    (ForeignPtr addr fp, I# ofs, I# len) ->
       C# (nextChar# addr fp ofs len)

unpackChar (C# x) = x

{-# NOINLINE nextChar# #-}
-- nextChar# :: Addr# -> ForeignPtrContents -> Int# -> Int# -> Char#
nextChar# addr fp ofs 0# = unpackChar '\000'
nextChar# addr fp ofs _ =
  unpackChar (BSI.inlinePerformIO $ 
  withForeignPtr (ForeignPtr addr fp) $ \ptr ->  
    fmap BSI.w2c (peekByteOff ptr (I# ofs)))
