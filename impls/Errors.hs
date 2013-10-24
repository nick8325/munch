{-# LANGUAGE Rank2Types, TypeFamilies, CPP, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Inner a b c)

type Inner a b c =
                 (b -> Reply a c -> a -> Maybe Error -> Reply a c) -- ok: success
              -> Reply a c                                         -- err: backtracking failure
              -> a -> Maybe Error -> Reply a c

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \ok err inp here failed -> p ok err inp here failed

{-# INLINE parsec #-}
parsec :: (forall c. Inner a b c) -> Parsec a b
parsec p = Parsec (eta p)

{-# INLINE runParsec #-}
runParsec :: Parsec a b -> Inner a b c
runParsec (Parsec p) = eta p

type Reply a b = Failed -> Result a b
data Failed = Nil | Cons_ {-# UNPACK #-} !Int (Maybe Error) Failed deriving Show

data Result a b = Ok b | Error Failed deriving Show
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
parseError :: Stream a => [Error] -> Parsec a b
parseError [] = pzero
parseError (e:_) = putError (Just e) `parsecBind` const pzero

{-# INLINE putError #-}
putError e = parsec (\ok err inp _ -> ok () err inp e)

{-# INLINE pzero #-}
pzero :: Stream a => Parsec a b
pzero = parsec (\ok err inp here failed -> err (Cons_ (pos inp) here failed))

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a b -> String -> Parsec a b
#ifndef IGNORE_LABELS
p <?> text = putError (Just (Expected text)) `parsecBind` const p
#else
p <?> text = p
#endif

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\ok -> ok x)

{-# INLINE parsecBind #-}
x `parsecBind` f = parsec (\ok -> runParsec x (\y -> runParsec (f y) ok))

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 = parsec (\ok err inp here ->
  runParsec m1 ok (runParsec m2 ok err inp here) inp here)

run :: Stream a => Parsec a b -> a -> Result a b
run p x = runParsec p ok err x Nothing Nil
  where ok x _ _ _ _ = Ok x
        err = Error

{-# INLINE cut #-}
cut :: Stream a => Parsec a ()
cut = parsec (\ok err inp here failed -> ok () Error inp here Nil)

{-# INLINE cut' #-}
cut' :: Stream a => Parsec a b -> Parsec a b
cut' p = parsec (\ok err inp here failed -> runParsec p (\x _ inp' here _ -> ok x err inp' here failed) err inp here failed)

checkpoint = return ()
progress = return ()
