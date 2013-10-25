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
    here :: p a,
    feed :: Maybe (Token (StreamType p)) -> Result p a
  }

data Result p a =
    Ok a
  | Error
  | Block (p a)

{-# INLINE active #-}
active :: p a -> Look p a
active p =
  Look {
    here = p,
    feed = \_ -> Block p
    }

{-# INLINE lookReturn #-}
lookReturn :: Parser p => a -> Look p a
lookReturn x =
  Look {
    here = return x,
    feed = \_ -> Ok x
    }

{-# INLINE lookBind #-}
lookBind :: Parser p => Look p a -> (a -> Look p b) -> Look p b
x `lookBind` f =
  Look {
    here = here x >>= here . f,
    feed = \t ->
      case feed x t of
        Ok x -> feed (f x) t
        Error -> Error
        Block p -> Block (p >>= here . f)
    }

{-# INLINE lookZero #-}
lookZero :: Parser p => Look p a
lookZero =
  Look {
    here = mzero,
    feed = \_ -> Error
    }

{-# INLINE activate #-}
activate :: Parser p => (Maybe (Token (StreamType p)) -> Result p a) -> p a
activate p = do
  t <- peek
  case p t of
    Ok x -> return x
    Error -> mzero
    Block q -> q

{-# INLINE lookChoice #-}
lookChoice :: Parser p => Look p a -> Look p a -> Look p a
p `lookChoice` q = 
  Look {
    here = activate (p `resultChoice` q),
    feed = p `resultChoice` q
    }

{-# INLINE resultChoice #-}
resultChoice :: Parser p => Look p a -> Look p a -> Maybe (Token (StreamType p)) -> Result p a
p `resultChoice` q = \t ->
  case (feed p t, feed q t) of
    (Ok x, _) -> Ok x
    (Error, x) -> x
    (x@Block{}, Error) -> x
    (Block p, Ok x) -> Block (p `mplus` return x)
    (Block p, Block q) -> Block (p `mplus` q)

{-# INLINE lookPeek #-}
lookPeek :: Parser p => Look p (Maybe (Token (StreamType p)))
lookPeek =
  Look {
    here = peek,
    feed = Ok
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
  Look {
    here = success (here p),
    feed = \t -> resultSuccess (feed p t)
    }

{-# INLINE resultSuccess #-}
resultSuccess :: Parser p => Result p a -> Result p a
resultSuccess (Ok x) = Ok x
resultSuccess Error = error "success called on failing parser"
resultSuccess (Block p) = Block (success p)

{-# INLINE lookRun #-}
lookRun :: Parser p => Look p a -> StreamType p -> Maybe a
lookRun p inp = run (here p) inp

instance Parser p => Functor (Look p) where fmap = liftM
instance Parser p => Applicative (Look p) where
  pure = return
  (<*>) = ap
instance Parser p => Alternative (Look p) where
  empty = mzero
  (<|>) = mplus
  some = parseSome
  many = parseMany
instance Parser p => Monad (Look p) where
  return = lookReturn
  (>>=) = lookBind
  fail _ = lookZero
instance Parser p => MonadPlus (Look p) where
  mzero = lookZero
  mplus = lookChoice
instance Parser p => Parser (Look p) where
  type StreamType (Look p) = StreamType p
  peek = lookPeek
  getInput = lookGetInput
  putInput = lookPutInput
  success = lookSuccess
  run = lookRun