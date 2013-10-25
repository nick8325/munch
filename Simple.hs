-- A simple parser implementation.
{-# LANGUAGE TypeFamilies, Rank2Types #-}
module Simple where

import Class
import Stream
import InstanceHelpers
import Control.Applicative
import Control.Monad

newtype Simple s a = Simple (forall r. CPS s a r)

type CPS s a r =
     (a -> s -> Reply r) -- ok: success
  -> Reply r             -- err: failure
  -> s -> Reply r

type Reply a = Result a
data Result a = Ok a | Error deriving Show

-- Eta-expanding smart constructors.
{-# INLINE cpsEta #-}
cpsEta :: CPS s a r -> CPS s a r
cpsEta p = \ok err inp -> p (\x inp' -> ok x inp') err inp

{-# INLINE parser #-}
parser :: (forall r. CPS s a r) -> Simple s a
parser p = Simple (cpsEta p)

{-# INLINE runParser #-}
runParser :: Simple s a -> CPS s a r
runParser (Simple p) = cpsEta p

{-# INLINE parserReturn #-}
parserReturn :: a -> Simple s a
parserReturn x = parser (\ok err -> ok x)

{-# INLINE parserBind #-}
parserBind :: Simple s a -> (a -> Simple s b) -> Simple s b
p `parserBind` f =
  parser (\ok err -> runParser p (\x -> runParser (f x) ok err) err)

{-# INLINE parserZero #-}
parserZero :: Simple s a
parserZero = parser (\ok err inp -> err)

{-# INLINE parserChoice #-}
parserChoice :: Simple s a -> Simple s a -> Simple s a
p `parserChoice` q =
  parser (\ok err inp -> runParser p ok (runParser q ok err inp) inp)

{-# INLINE parserGetInput #-}
parserGetInput :: Simple s s
parserGetInput = parser (\ok err inp -> ok inp inp)

{-# INLINE parserPutInput #-}
parserPutInput :: s -> Simple s ()
parserPutInput inp = parser (\ok err _ -> ok () inp)

{-# INLINE parserSuccess #-}
parserSuccess :: Simple s a -> Simple s a
parserSuccess p = parser (\ok err -> runParser p ok Error)

{-# INLINE parserRun #-}
parserRun :: Simple s a -> s -> Maybe a
parserRun p inp =
  case runParser p (\x _ -> Ok x) Error inp of
    Ok x -> Just x
    Error -> Nothing

instance Stream s => Functor (Simple s) where
  fmap = liftM
instance Stream s => Applicative (Simple s) where
  pure = return
  (<*>) = ap
  (*>) = (>>)
  (<*) = followedBy
instance Stream s => Alternative (Simple s) where
  empty = mzero
  (<|>) = mplus
  some = parseSome
  many = parseMany
instance Stream s => Monad (Simple s) where
  return = parserReturn
  (>>=) = parserBind
  fail _ = parserZero
instance Stream s => MonadPlus (Simple s) where
  mzero = parserZero
  mplus = parserChoice
instance Stream s => Parser (Simple s) where
  type StreamType (Simple s) = s
  getInput = parserGetInput
  putInput = parserPutInput
  success = parserSuccess
  run = parserRun