{-# LANGUAGE Rank2Types, TypeFamilies, CPP, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Inner a b c)

type Inner a b c =
                 Reply c                        -- fatal: non-backtracking failure
              -> (b -> Reply c -> a -> Reply c) -- ok: success
              -> Reply c                        -- err: backtracking failure
              -> a -> Reply c

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \fatal ok err inp exp -> p fatal ok err inp exp

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
getInput = parsec (\fatal ok err inp -> ok inp err inp)

{-# INLINE putInput #-}
putInput :: a -> Parsec a ()
putInput inp = parsec (\fatal ok err _ -> ok () err inp)

{-# INLINE parseError #-}
parseError :: [Error] -> Parsec a b
parseError e = parsec (\fatal ok err inp exp -> err (e ++ exp))

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a b -> String -> Parsec a b
#ifndef IGNORE_LABELS
p <?> text = Parsec (\fatal ok err inp exp ->
  runParsec p fatal ok err inp (expectedMsg text:exp))
#else
p <?> text = p
#endif

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\fatal ok -> ok x)

{-# INLINE parsecBind #-}
x `parsecBind` f = parsec (\fatal ok -> runParsec x fatal (\y -> runParsec (f y) fatal ok))

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 = parsec (\fatal ok err inp ->
  runParsec m1 fatal ok (runParsec m2 fatal ok err inp) inp)

run :: Parsec a b -> a -> Result b
run p x = runParsec p err ok err x []
  where ok x _ _ _ = Ok x
        err = Error

{-# INLINE cut #-}
cut :: Parsec a ()
cut = parsec (\fatal ok err inp exp -> ok () fatal inp [])

{-# INLINE try #-}
try :: Parsec a b -> Parsec a b
try p = parsec (\fatal ok err -> runParsec p err ok err)

checkpoint = return ()
progress = return ()
