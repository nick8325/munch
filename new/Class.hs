-- A class for parsers.
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Class where

import Control.Applicative
import Control.Monad
import Stream

class (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m,
       Stream (StreamType m)) => Parser m where
  type StreamType m
  
  {-# INLINEABLE peek #-}
  peek :: m (Maybe (Token (StreamType m)))
  peek = fmap (fmap fst . uncons) getInput
    
  getInput :: m (StreamType m)
  putInput :: StreamType m -> m ()
  
  {-# INLINEABLE success #-}
  success :: m a -> m a
  success = id
  
  run :: m a -> StreamType m -> Maybe a