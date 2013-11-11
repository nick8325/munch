{-# LANGUAGE TypeFamilies #-}

module Stream where

import qualified Data.ByteString.Char8 as B
import Data.Char

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

newtype Classify a = Classify a
data CatChar = CatChar { cat :: GeneralCategory, char :: {-# UNPACK #-} !Char }

instance (Token a ~ Char, Stream a) => Stream (Classify a) where
  type Token (Classify a) = CatChar
  {-# INLINE uncons #-}
  uncons (Classify x)=
    case uncons x of
      Nothing -> Nothing
      Just (x, xs) -> Just (CatChar (generalCategory x) x, Classify xs)
  {-# INLINE pos #-}
  pos (Classify x) = pos x