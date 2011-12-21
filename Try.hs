{-# LANGUAGE Rank2Types, TypeFamilies, CPP #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Inner a b c)

type Inner a b c =
                 (b -> Reply c -> Reply c -> a -> Reply c) -- ok: success
              -> Reply c                        -- err: backtracking failure
              -> Reply c                        -- fatal: non-backtracking failure
              -> a -> Reply c

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \ok err fatal inp exp -> p ok err fatal inp exp

{-# INLINE parsec #-}
parsec :: (forall c. Inner a b c) -> Parsec a b
parsec p = Parsec (eta p)

{-# INLINE runParsec #-}
runParsec :: Parsec a b -> Inner a b c
runParsec (Parsec p) = eta p

type Reply a = [Error] -> a

data Result a = Ok a | Error [Error] deriving Show
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
parseError e = parsec (\ok err fatal inp exp -> err (e ++ exp))

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a b -> String -> Parsec a b
#ifndef IGNORE_LABELS
p <?> text = Parsec (\ok err fatal inp exp ->
  runParsec p ok err fatal inp (expectedMsg text:exp))
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

run :: Parsec a b -> a -> Result b
run p x = runParsec p ok err err x []
  where ok x _ _ _ _ = Ok x
        err = Error

{-# INLINE cut #-}
cut :: Parsec a ()
cut = parsec (\ok err fatal inp exp -> ok () fatal fatal inp [])

{-# INLINE try #-}
try :: Parsec a b -> Parsec a b
try p = parsec (\ok err fatal -> runParsec p ok err err)