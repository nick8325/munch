{-# LANGUAGE TypeFamilies #-}
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

instance Stream BS.ByteString where
  type Token BS.ByteString = Char
  {-# INLINE primToken #-}
  primToken bs ok err _ =
    case BS.uncons bs of
      Nothing -> err
      Just (c, cs) -> ok cs c

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

class UnboxMaybe a where
  type UnboxedMaybe a
  unbox :: Maybe a -> UnboxedMaybe a
  box :: UnboxedMaybe a -> Maybe a

instance UnboxMaybe Char where
  type UnboxedMaybe Char = Int
  {-# INLINE unbox #-}
  unbox Nothing = maxBound
  unbox (Just x) = ord x
  {-# INLINE box #-}
  box x | x == maxBound = Nothing
        | otherwise = Just (unsafeChr x)

data Peek a = Peek !(UnboxedMaybe a) a

-- instance (Stream a, UnboxMaybe (Token a)) => Stream (Peek a) where
--   type Token (Peek a) = Token a
--   {-# INLINE primToken #-}
--   primToken (Peek x xs) ok err fail =
--     primToken -- ?????????
