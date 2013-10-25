-- Adding the look 1-token lookahead feature to a parser.
{-# LANGUAGE TypeFamilies #-}
module Look2 where

import Class
import Stream
import InstanceHelpers
import Control.Applicative
import Control.Monad

-- DOESN'T WORK because:
-- when we have a sequence of binds, we don't want any datatypes
-- getting in the way---it should only happen when we use choice
data Look p a =
    Peek (Maybe (Token (StreamType p)) -> Result p a)
  | Here (Result p a)

data Result p a =
    Ok a
  | Error
  | Block (p a)

{-# INLINE CONLIKE lookEta #-}
lookEta :: Parser p => Look p a -> Look p a
lookEta (Peek f) = Peek (\t -> resultEta (f t))
lookEta (Here r) = Here (resultEta r)

{-# INLINE CONLIKE resultEta #-}
resultEta :: Parser p => Result p a -> Result p a
resultEta (Ok x) = Ok x
resultEta Error = Error
resultEta (Block p) = Block (eta p)

{-# INLINE CONLIKE activateWith #-}
activateWith :: Parser p => Maybe (Token (StreamType p)) -> Look p a -> p a
activateWith t (Here r) = activateResult r
activateWith t (Peek f) = activateResult (f t)

{-# INLINE CONLIKE activate #-}
activate :: Parser p => Look p a -> p a
activate (Here r) = activateResult r
activate (Peek f) = peek >>= activateResult . f

{-# INLINE CONLIKE activateResult #-}
activateResult :: Parser p => Result p a -> p a
activateResult (Ok x) = return x
activateResult Error = mzero
activateResult (Block p) = p

{-# INLINE CONLIKE lookReturn #-}
lookReturn :: Parser p => a -> Look p a
lookReturn x = Here (Ok x)

{-# INLINE CONLIKE lookBind #-}
lookBind :: Parser p => Look p a -> (a -> Look p b) -> Look p b
Here (Ok x) `lookBind` f = f x
Here Error `lookBind` f = Here Error
Here (Block p) `lookBind` f = Here (Block (p >>= activate . f))
Peek x `lookBind` f =
  Peek $ \t ->
    case x t of
      Ok x -> unpeek t (f x)
      Error -> Error
      Block p -> Block (p >>= activate . f)

{-# INLINE CONLIKE unpeek #-}
unpeek :: Maybe (Token (StreamType p)) -> Look p a -> Result p a
unpeek _ (Here r) = r
unpeek t (Peek f) = f t

{-# INLINE CONLIKE lookZero #-}
lookZero :: Parser p => Look p a
lookZero = Here Error

{-# INLINE CONLIKE lookChoice #-}
lookChoice :: Parser p => Look p a -> Look p a -> Look p a
x@(Here (Ok _)) `lookChoice` _ = x
Here Error `lookChoice` y = y
x `lookChoice` Here Error = x
Here x `lookChoice` Here y = Here (x `resultChoice` y)
Here x `lookChoice` Peek y = Peek (\t -> x `resultChoice` y t)
Peek x `lookChoice` Here y = Peek (\t -> x t `resultChoice` y)
Peek x `lookChoice` Peek y = Peek (\t -> x t `resultChoice` y t)

{-# INLINE CONLIKE resultChoice #-}
resultChoice :: Parser p => Result p a -> Result p a -> Result p a
x@(Ok _) `resultChoice` _ = x
Error `resultChoice` y = y
x `resultChoice` Error = x
Block p `resultChoice` x = Block (p `mplus` activateResult x)

{-# INLINE CONLIKE lookPeek #-}
lookPeek :: Parser p => Look p (Maybe (Token (StreamType p)))
lookPeek = Peek Ok

{-# INLINE CONLIKE lookGetInput #-}
lookGetInput :: Parser p => Look p (StreamType p)
lookGetInput = Here (Block getInput)

{-# INLINE CONLIKE lookPutInput #-}
lookPutInput :: Parser p => StreamType p -> Look p ()
lookPutInput inp = Here (Block (putInput inp))

{-# INLINE CONLIKE lookSuccess #-}
lookSuccess :: Parser p => Look p a -> Look p a
lookSuccess (Here r) = Here (resultSuccess r)
lookSuccess (Peek f) = Peek (\t -> resultSuccess (f t))

{-# INLINE CONLIKE resultSuccess #-}
resultSuccess :: Parser p => Result p a -> Result p a
resultSuccess (Block p) = Block (success p)
resultSuccess x = x

{-# INLINE CONLIKE lookProgress #-}
lookProgress :: Parser p => Look p a -> Look p a
lookProgress (Here x) = Here (resultProgress x)
lookProgress (Peek f) = Peek (\t -> resultProgress (f t))

{-# INLINE CONLIKE resultProgress #-}
resultProgress :: Parser p => Result p a -> Result p a
resultProgress (Ok _) = Error
resultProgress Error = Error
resultProgress (Block p) = Block (progress p)

{-# INLINE CONLIKE lookRun #-}
lookRun :: Parser p => Look p a -> StreamType p -> Maybe a
lookRun p inp = run (activate p) inp

instance Parser p => Functor (Look p) where
  {-# INLINE CONLIKE fmap #-}
  fmap f mx = do { x <- mx; return (f x) }
instance Parser p => Applicative (Look p) where
  {-# INLINE CONLIKE pure #-}
  pure x = return x
  {-# INLINE CONLIKE (<*>) #-}
  mf <*> mx = do { f <- mf; x <- mx; return (f x) }
  {-# INLINE CONLIKE (*>) #-}
  x *> y = x >> y
  {-# INLINE CONLIKE (<*) #-}
  mx <* my = do { x <- mx; my; return x }
instance Parser p => Alternative (Look p) where
  {-# INLINE CONLIKE empty #-}
  empty = mzero
  {-# INLINE CONLIKE (<|>) #-}
  x <|> y = x `mplus` y
  {-# INLINE CONLIKE some #-}
  some p = parseSome p
  {-# INLINE CONLIKE many #-}
  many p = parseMany p
instance Parser p => Monad (Look p) where
  {-# INLINE CONLIKE return #-}
  return x = lookReturn x
  {-# INLINE CONLIKE (>>=) #-}
  x >>= f = x `lookBind` f
  {-# INLINE CONLIKE fail #-}
  fail _ = lookZero
instance Parser p => MonadPlus (Look p) where
  {-# INLINE CONLIKE mzero #-}
  mzero = lookZero
  {-# INLINE CONLIKE mplus #-}
  x `mplus` y = x `lookChoice` y
instance Parser p => Parser (Look p) where
  type StreamType (Look p) = StreamType p
  {-# INLINE CONLIKE peek #-}
  peek = lookPeek
  {-# INLINE CONLIKE getInput #-}
  getInput = lookGetInput
  {-# INLINE CONLIKE putInput #-}
  putInput x = lookPutInput x
  {-# INLINE CONLIKE success #-}
  success p = lookSuccess p
  {-# INLINE CONLIKE progress #-}
  progress p = lookProgress p
  {-# INLINE CONLIKE run #-}
  run p s = lookRun p s
  {-# INLINE CONLIKE eta #-}
  eta = lookEta