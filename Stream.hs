{-# LANGUAGE TypeFamilies #-}

module Stream where

import qualified Data.ByteString.Char8 as B

class Stream a where
  type Token a
  uncons :: a -> Maybe (Token a, a)
  pos :: a -> Int

instance Stream [a] where
  type Token [a] = a
  {-# INLINE uncons #-}
  uncons [] = Nothing
  uncons (x:xs) = Just (x, xs)
  pos x = - (length x)

instance Stream B.ByteString where
  type Token B.ByteString = Char
  {-# INLINE uncons #-}
  uncons = B.uncons
  {-# INLINE pos #-}
  pos x = - (B.length x)