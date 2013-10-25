{-# LANGUAGE TypeFamilies, BangPatterns, MagicHash, FlexibleInstances #-}
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
import Unsafe.Coerce

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

  newtype Fun BS.ByteString a = FB (Addr# -> Any -> Int# -> Int# -> a)
  {-# INLINE abs #-}
  abs f =
    FB (\addr fp ofs len ->
      f (BSI.fromForeignPtr (ForeignPtr addr (unsafeCoerce fp)) (I# ofs) (I# len)))
  {-# INLINE app #-}
  app (FB f) bs =
    case BSI.toForeignPtr bs of
      (ForeignPtr addr fp, I# ofs, I# len) ->
        f addr (unsafeCoerce fp) ofs len

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
  hd (Cons c _) = c
  newtype Fun Chars a = CF (Char# -> Chars -> a)
  abs f = CF (\c# cs -> f (Cons (C# c#) cs))
  app (CF f) (Cons (C# c#) cs) = f c# cs

instance Stream [Char] where
  type Token [Char] = Char
  {-# INLINE primToken #-}
  primToken [] _ err _ = err
  primToken (x:xs) ok _ _ = ok xs x
  hd [] = '\000'
  hd (x:xs) = x

data CharBS = CharBS {-# UNPACK #-} !Char {-# UNPACK #-} !BS.ByteString

pack bs = CharBS (nextChar bs) bs
unpack (CharBS _ s) = s

instance Stream CharBS where
  type Token CharBS = Char
  
  {-# INLINE primToken #-}
  primToken (CharBS !c s) ok err _
    | c == '\000' && BS.null s = err
    | otherwise =
        (ok $! CharBS (nextChar (BSU.unsafeTail s)) (BSU.unsafeTail s)) c

  {-# INLINE hd #-}
  hd (CharBS !c _) = c

  newtype Fun CharBS a = F (Char# -> Addr# -> Any -> Int# -> Int# -> a)
  {-# INLINE abs #-}
  abs f =
    F (\x addr fp ofs len ->
      f (CharBS (C# x) (BSI.fromForeignPtr (ForeignPtr addr (unsafeCoerce fp)) (I# ofs) (I# len))))
  {-# INLINE app #-}
  app (F f) (CharBS (C# x) bs) =
    case BSI.toForeignPtr bs of
      (ForeignPtr addr fp, I# ofs, I# len) ->
        f x addr (unsafeCoerce fp) ofs len

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