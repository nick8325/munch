-- A class for parsers.
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Class where

import Control.Applicative
import Control.Monad
import Stream

data Result a = Ok a | Error [Error] deriving Show
data Error = Expected Int String deriving Show

class (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m,
       Stream (StreamType m)) => Parser m where
  type StreamType m

  {-# INLINE peek #-}
  peek :: m (Maybe (Token (StreamType m)))
  peek = fmap (fmap fst . uncons) getInput

  getInput :: m (StreamType m)
  putInput :: StreamType m -> m ()

  (<?>) :: m a -> String -> m a
  p <?> x = p

  expected :: (Int -> [Error] -> [Error]) -> m a -> m a
  expected _ p = p

  success :: m a -> m a
  success = id

  -- Like Parsec's 'nonempty', but does not check anything.
  -- Used to help the simplifier.
  progress :: m a -> m a
  progress = id

  run :: m a -> StreamType m -> Result a