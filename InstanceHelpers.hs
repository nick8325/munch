module InstanceHelpers where

import Control.Applicative
import Control.Monad
import Class

{-# INLINE followedBy #-}
followedBy :: Monad m => m a -> m b -> m a
p `followedBy` q = do { x <- p; q; return x }

{-# INLINE parseSome #-}
parseSome :: Parser p => p a -> p [a]
parseSome p = do { x <- p; xs <- many p; return (x:xs) }

{-# INLINE parseMany #-}
parseMany :: Parser p => p a -> p [a]
parseMany p = p' where p' = success $ liftM2 (:) p p' <|> return []
  -- Stack overflow-avoiding version:
  -- many p = liftM reverse (p' [])
  --   where p' !xs = do { x <- nonempty p; p' (x:xs) } `mplus` return xs
