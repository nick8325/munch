{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module BS where

import Parsec
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSW
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Internal as BSI
import Data.Word
import Foreign.Storable
import Foreign.ForeignPtr
import qualified Data.Text as T

instance Stream BS.ByteString where
  type Token BS.ByteString = Char
  {-# INLINE primToken #-}
  primToken bs ok err _ =
    case BS.uncons bs of
      Nothing -> err
      Just (c, cs) -> ok cs c
  -- type Position BS.ByteString = BS.ByteString
  -- position = id

instance Stream T.Text where
  type Token T.Text = Char
  {-# INLINE primToken #-}
  primToken t ok err _ =
    case T.uncons t of
      Nothing -> err
      Just (c, cs) -> ok cs c

-- uncons :: BS.ByteString -> Maybe (Char, BS.ByteString)
-- uncons (BSI.PS x s l)
--     | l <= 0    = Nothing
--     | otherwise =
--       BSI.inlinePerformIO $ do
--         c <- peekByteOff (unsafeForeignPtrToPtr x) s
--         touchForeignPtr x
--         return (Just (c, BSI.PS x (s+1) (l-1)))

withUnsafeForeignPtr x io = io (unsafeForeignPtrToPtr x)

-- {-# NOINLINE CONLIKE uncons #-}
-- uncons :: BS.ByteString -> Maybe (Char, BS.ByteString)
-- uncons (BSI.PS x s l)
--     | l <= 0    = Nothing
--     | otherwise =
--         BSI.inlinePerformIO $
--         withUnsafeForeignPtr x $ \p -> do
--           c <- peekByteOff p s
--           return (Just (BSI.w2c c, BSI.PS x (s+1) (l-1)))

data CharBS = CharBS {-# UNPACK #-} !Char {-# UNPACK #-} !BS.ByteString

instance Stream CharBS where
  type Token CharBS = Char
  {-# INLINE primToken #-}
  primToken (CharBS _ bs) ok err _ | BS.null bs = err
  primToken (CharBS c bs) ok err _ = ok (charBS (BSU.unsafeTail bs)) c
  -- type Position CharBS = BS.ByteString
  -- position (CharBS _ bs) = bs

{-# INLINE charBS #-}
charBS bs | BS.null bs = CharBS '\000' bs
          | otherwise = CharBS (BSI.w2c (BSU.unsafeHead bs)) bs