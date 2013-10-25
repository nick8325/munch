{-# LANGUAGE Rank2Types, TypeFamilies, CPP, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Inner a b c)

type Inner a b c =
                 (b -> Reply a c -> a -> Reply a c) -- ok: success
              -> Reply a c                        -- err: backtracking failure
              -> a
              -> Reply a c

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \ok err inp checkpoint exp -> p ok err inp checkpoint exp

{-# INLINE parsec #-}
parsec :: (forall c. Inner a b c) -> Parsec a b
parsec p = Parsec (eta p)

{-# INLINE runParsec #-}
runParsec :: Parsec a b -> Inner a b c
runParsec (Parsec p) = eta p

type Reply a b = Checkpoint a b -> [Error] -> Result a b
newtype Checkpoint a b = Checkpoint (a -> Checkpoint a b -> Result a b)

data Result a b = Ok b | Error (Checkpoint a b) [Error] | More (Checkpoint a b)
instance Show b => Show (Result a b) where
  show = show . f
    where f (Ok x) = Right x
          f (Error _ xs) = Left xs
          f More{} = Left [Message "more"]
data Error = Expected String | Message String deriving Show

errorMsg = Message
expectedMsg = Expected

{-# INLINE getInput #-}
getInput :: Parsec a a
getInput = parsec (\ok err inp -> ok inp err inp)

{-# INLINE putInput #-}
putInput :: a -> Parsec a ()
putInput inp = parsec (\ok err _ -> ok () err inp)

{-# INLINE parseError #-}
parseError :: [Error] -> Parsec a b
parseError e = parsec (\ok err inp more exp -> err more (e ++ exp))

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a b -> String -> Parsec a b
#ifndef IGNORE_LABELS
p <?> text = Parsec (\ok err inp more exp ->
  runParsec p ok err inp more (expectedMsg text:exp))
#else
p <?> text = p
#endif

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\ok -> ok x)

{-# INLINE parsecBind #-}
x `parsecBind` f = parsec (\ok -> runParsec x (\y -> runParsec (f y) ok))

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 = parsec (\ok err inp ->
  runParsec m1 ok (runParsec m2 ok err inp) inp)

run :: Stream a => Parsec a b -> a -> Result a b
run p x = runParsec (checkpoint `parsecBind` const p) ok err x undefined []
  where ok x _ _ _ _ = Ok x
        err = Error

{-# INLINE cut #-}
cut :: Parsec a ()
cut = parsec (\ok err inp cp exp -> ok () Error inp cp [])

{-# INLINE checkpoint #-}
checkpoint :: Stream a => Parsec a ()
checkpoint = parsec (\ok err inp _ exp ->
--  let rest = \inp checkpoint -> ok () err inp checkpoint exp
--  in rest inp (More inp rest))
  ok () err inp (Checkpoint (\inp' checkpoint -> ok () err (inp `append` inp') checkpoint exp)) exp)

progress = return ()