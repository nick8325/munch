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
     (a -> s -> Reply r)  -- ok: success
  -> ([Error] -> Reply r) -- err: failure
  -> [Error] -> s -> Reply r

type Reply a = () -> Result a

-- Eta-expanding smart constructors.
{-# INLINE cpsEta #-}
cpsEta :: CPS s a r -> CPS s a r
cpsEta p = \ok err errs inp x -> p ok (\errs x -> err errs x) errs inp x

{-# INLINE parser #-}
parser :: (forall r. CPS s a r) -> Simple s a
parser p = Simple (cpsEta p)

{-# INLINE runParser #-}
runParser :: Simple s a -> CPS s a r
runParser (Simple p) = cpsEta p

{-# INLINE parserReturn #-}
parserReturn :: a -> Simple s a
parserReturn x = parser (\ok err errs -> ok x)

{-# INLINE parserBind #-}
parserBind :: Simple s a -> (a -> Simple s b) -> Simple s b
p `parserBind` f =
  parser (\ok err errs -> runParser p (\x -> runParser (f x) ok err errs) err errs)

{-# INLINE parserZero #-}
parserZero :: Simple s a
parserZero = parser (\ok err errs inp -> err errs)

{-# INLINE parserChoice #-}
parserChoice :: Simple s a -> Simple s a -> Simple s a
p `parserChoice` q =
  parser (\ok err errs inp -> runParser p ok (\errs -> runParser q ok err errs inp) errs inp)

{-# INLINE parserGetInput #-}
parserGetInput :: Simple s s
parserGetInput = parser (\ok err errs inp -> ok inp inp)

{-# INLINE parserPutInput #-}
parserPutInput :: s -> Simple s ()
parserPutInput inp = parser (\ok err _ _ -> ok () inp)

{-# INLINE parserNote #-}
parserNote :: Stream s => Simple s a -> String -> Simple s a
parserNote p x = parserExpected (\n errs -> Expected n x:errs) p

{-# INLINE parserExpected #-}
parserExpected :: Stream s => (Int -> [Error] -> [Error]) -> Simple s a -> Simple s a
parserExpected f p = parser (\ok err errs inp -> runParser p ok err (f (pos inp) errs) inp)

{-# INLINE parserSuccess #-}
parserSuccess :: Simple s a -> Simple s a
parserSuccess p = parser (\ok err errs -> runParser p ok (\errs _ -> Error errs) [])

{-# INLINE parserRun #-}
parserRun :: Simple s a -> s -> Result a
parserRun p inp =
  runParser p (\x _ _ -> Ok x) (\errs _ -> Error errs) [] inp ()

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
  (<?>) = parserNote
  expected = parserExpected
  success = parserSuccess
  run = parserRun