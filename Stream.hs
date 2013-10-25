{-# LANGUAGE TypeFamilies #-}

module Stream where

import qualified Data.ByteString.Char8 as B

class Stream a where
  type Token a
  uncons :: a -> Maybe (Token a, a)

instance Stream [a] where
  type Token [a] = a
  {-# INLINE uncons #-}
  uncons [] = Nothing
  uncons (x:xs) = Just (x, xs)

instance Stream B.ByteString where
  type Token B.ByteString = Char
  {-# INLINE uncons #-}
  uncons = B.uncons