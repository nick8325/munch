{-# LANGUAGE TypeFamilies #-}

module Stream where

import qualified Data.ByteString.Char8 as B

class Stream a where
  type Token a
  {-# INLINE hd #-}
  hd :: a -> Maybe (Token a)
  hd = fmap fst . uncons
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

data HeadedBS = Headed {-# UNPACK #-} !Char {-# UNPACK #-} !B.ByteString

{-# INLINE headed #-}
headed :: B.ByteString -> HeadedBS
headed x =
  case B.uncons x of
    Nothing -> Headed '\000' x
    Just (x, xs) -> Headed x xs

instance Stream HeadedBS where
  type Token HeadedBS = Char
  {-# INLINE hd #-}
  hd (Headed c _) = Just c
  {-# INLINE uncons #-}
  uncons (Headed c bs) = Just (c, headed bs)