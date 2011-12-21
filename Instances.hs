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