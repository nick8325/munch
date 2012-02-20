{-# LANGUAGE Rank2Types, TypeFamilies, CPP #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Inner a b c)

type Inner a b c =
                 (b -> Reply a c -> Reply a c -> a -> Reply a c) -- ok: success
              -> Reply a c                        -- err: backtracking failure
              -> Reply a c                        -- fatal: non-backtracking failure
              -> a -> Reply a c

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \ok err fatal inp more exp -> p ok err fatal inp more exp

{-# INLINE parsec #-}
parsec :: (forall c. Inner a b c) -> Parsec a b
parsec p = Parsec (eta p)

{-# INLINE runParsec #-}
runParsec :: Parsec a b -> Inner a b c
runParsec (Parsec p) = eta p

type Reply a b = (a -> b) -> [Error] -> b

data Result a b = Ok b | Error [Error] | More (a -> Result a b)
instance Show b => Show (Result a b) where
  show = show . f
    where f (Ok x) = Right x
          f (Error xs) = Left xs
          f (More x) = Left [Message "more"]
data Error = Expected String | Message String deriving Show

errorMsg = Message
expectedMsg = Expected

{-# INLINE getInput #-}
getInput :: Parsec a a
getInput = parsec (\ok err fatal inp -> ok inp err fatal inp)

{-# INLINE putInput #-}
putInput :: a -> Parsec a ()
putInput inp = parsec (\ok err fatal _ -> ok () err fatal inp)

{-# INLINE parseError #-}
parseError :: [Error] -> Parsec a b
parseError e = parsec (\ok err fatal inp more exp -> err more (e ++ exp))

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a b -> String -> Parsec a b
#ifndef IGNORE_LABELS
p <?> text = Parsec (\ok err fatal inp more exp ->
  runParsec p ok err fatal inp more (expectedMsg text:exp))
#else
p <?> text = p
#endif

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\ok -> ok x)

{-# INLINE parsecBind #-}
x `parsecBind` f = parsec (\ok -> runParsec x (\y -> runParsec (f y) ok))

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 = parsec (\ok err fatal inp ->
  runParsec m1 ok (runParsec m2 ok err fatal inp) fatal inp)

run :: Parsec a b -> a -> Result a b
run p x = runParsec p ok err err x (run p) []
  where ok x _ _ _ _ _ = Ok x
        err _ = Error

{-# INLINE cut #-}
cut :: Parsec a ()
cut = parsec (\ok err fatal inp more exp -> ok () fatal fatal inp more [])

{-# INLINE try #-}
try :: Parsec a b -> Parsec a b
try p = parsec (\ok err fatal -> runParsec p ok err err)

{-# INLINE checkpoint #-}
checkpoint :: Parsec a ()
checkpoint = parsec (\ok err fatal inp more exp ->
  let go inp = ok () err fatal inp go exp
  in go inp)

progress = return ()
