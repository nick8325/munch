-- Adding the look 1-token lookahead feature to a parser.
{-# LANGUAGE TypeFamilies #-}
module Look where

import Class
import Stream
import InstanceHelpers
import Control.Applicative
import Control.Monad

data Look p a =
  Look {
    here_ :: p a,
    look_ :: Maybe (Token (StreamType p)) -> Kind a,
    feed_ :: Maybe (Token (StreamType p)) -> p a
  }

{-# INLINE lookEta #-}
lookEta :: Parser p => Look p a -> Look p a
lookEta p@Look{} =
  Look {
    here_ = here p,
    look_ = look p,
    feed_ = feed p
    }

{-# INLINE here #-}
here p = eta (here_ p)
{-# INLINE look #-}
look p = \t -> look_ p t
{-# INLINE feed #-}
feed p = \t -> eta (feed_ p t)

data Kind a =
    Ok a
  | Error
  | Block

{-# INLINE active #-}
active :: Parser p => p a -> Look p a
active p =
  eta $ Look {
    here_ = p,
    look_ = \_ -> Block,
    feed_ = \_ -> p
    }

{-# INLINE lookReturn #-}
lookReturn :: Parser p => a -> Look p a
lookReturn x =
  eta $ Look {
    here_ = return x,
    look_ = \_ -> Ok x,
    feed_ = \_ -> return x
    }

{-# INLINE[0] lookBind #-}
lookBind :: Parser p => Look p a -> (a -> Look p b) -> Look p b
x `lookBind` f =
  eta $ Look {
    here_ = hereBind x f,
    look_ = kindBind x f,
    feed_ = feedBind x f
    }

{-# INLINE hereBind #-}
hereBind x f = here x >>= here . f

{-# INLINE kindBind #-}
kindBind x f = \t ->
  case look x t of
    Ok x -> look (f x) t
    Error -> Error
    Block -> Block

{-# INLINE[0] feedBind #-}
feedBind x f = \t ->
  case look x t of
    Ok x -> feed (f x) t
    Error -> mzero
    Block -> feed x t >>= here . f

{-# INLINE lookZero #-}
lookZero :: Parser p => Look p a
lookZero =
  eta $ Look {
    here_ = mzero,
    look_ = \_ -> Error,
    feed_ = \_ -> mzero
    }

{-# INLINE lookChoice #-}
lookChoice :: Parser p => Look p a -> Look p a -> Look p a
p `lookChoice` q = 
  eta $ Look {
    here_ = hereChoice p q,
    look_ = \t -> kindChoice p q t,
    feed_ = \t -> feedChoice p q t
    }

{-# INLINE[0] hereChoice #-}
hereChoice p q = do { t <- peek; feedChoice p q t }

{-# INLINE kindChoice #-}
kindChoice :: Parser p => Look p a -> Look p a -> Maybe (Token (StreamType p)) -> Kind a
kindChoice p q t =
  case (look p t, look q t) of
    (Error, x) -> x
    (x, _) -> x

{-# INLINE feedChoice #-}
feedChoice :: Parser p => Look p a -> Look p a -> Maybe (Token (StreamType p)) -> p a
feedChoice p q t =
  case (look p t, look q t) of
    (Ok x, _) -> return x
    (Error, Ok x) -> return x
    (Error, Error) -> mzero
    (Error, Block) -> feed q t
    (Block, Ok x) -> feed p t `mplus` return x
    (Block, Error) -> feed p t
    (Block, Block) -> feed p t `mplus` feed q t

{-# INLINE lookPeek #-}
lookPeek :: Parser p => Look p (Maybe (Token (StreamType p)))
lookPeek =
  eta $ Look {
    here_ = peek,
    look_ = Ok,
    feed_ = return
    }

{-# INLINE lookGetInput #-}
lookGetInput :: Parser p => Look p (StreamType p)
lookGetInput = active getInput

{-# INLINE lookPutInput #-}
lookPutInput :: Parser p => StreamType p -> Look p ()
lookPutInput inp = active (putInput inp)

{-# INLINE lookSuccess #-}
lookSuccess :: Parser p => Look p a -> Look p a
lookSuccess p =
  eta $ Look {
    here_ = success (here p),
    look_ = look p,
    feed_ = success . feed p
    }

{-# INLINE lookProgress #-}
lookProgress :: Parser p => Look p a -> Look p a
lookProgress p =
  eta $ Look {
    here_ = progress (here p),
    look_ = kindProgress,
    feed_ = progress . feedProgress
    }
  where
    {-# INLINE kindProgress #-}
    kindProgress t =
      case look p t of
        Ok{} -> Error
        x -> x
    {-# INLINE feedProgress #-}
    feedProgress t =
      case look p t of
        Ok{} -> mzero
        _ -> feed p t

{-# INLINE lookRun #-}
lookRun :: Parser p => Look p a -> StreamType p -> Maybe a
lookRun p inp = run (here p) inp

instance Parser p => Functor (Look p) where
  {-# INLINE fmap #-}
  fmap f mx = do { x <- mx; return (f x) }
instance Parser p => Applicative (Look p) where
  {-# INLINE pure #-}
  pure x = return x
  {-# INLINE (<*>) #-}
  mf <*> mx = do { f <- mf; x <- mx; return (f x) }
  {-# INLINE (*>) #-}
  x *> y = x >> y
  {-# INLINE (<*) #-}
  mx <* my = do { x <- mx; my; return x }
instance Parser p => Alternative (Look p) where
  {-# INLINE empty #-}
  empty = mzero
  {-# INLINE (<|>) #-}
  x <|> y = x `mplus` y
  {-# INLINE some #-}
  some p = parseSome p
  {-# INLINE many #-}
  many p = parseMany p
instance Parser p => Monad (Look p) where
  {-# INLINE return #-}
  return x = lookReturn x
  {-# INLINE (>>=) #-}
  x >>= f = x `lookBind` f
  {-# INLINE fail #-}
  fail _ = lookZero
instance Parser p => MonadPlus (Look p) where
  {-# INLINE mzero #-}
  mzero = lookZero
  {-# INLINE mplus #-}
  x `mplus` y = x `lookChoice` y
instance Parser p => Parser (Look p) where
  type StreamType (Look p) = StreamType p
  {-# INLINE peek #-}
  peek = lookPeek
  {-# INLINE getInput #-}
  getInput = lookGetInput
  {-# INLINE putInput #-}
  putInput x = lookPutInput x
  {-# INLINE success #-}
  success p = lookSuccess p
  {-# INLINE progress #-}
  progress p = lookProgress p
  {-# INLINE run #-}
  run p s = lookRun p s
  {-# INLINE eta #-}
  eta = lookEta