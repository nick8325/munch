{-# LANGUAGE Rank2Types, TypeFamilies, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Inner a b c)

type Inner a b c =
                 (b -> Reply c -> a -> Reply c) -- ok: success
              -> Reply c                        -- err: backtracking failure
              -> a -> Reply c

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \ok err inp -> p ok err inp

{-# INLINE parsec #-}
parsec :: (forall c. Inner a b c) -> Parsec a b
parsec p = Parsec (eta p)

{-# INLINE runParsec #-}
runParsec :: Parsec a b -> Inner a b c
runParsec (Parsec p) = eta p

type Reply a = Result a

data Result a = Ok a | Error deriving Show

errorMsg = undefined
expectedMsg = undefined

{-# INLINE getInput #-}
getInput :: Parsec a a
getInput = parsec (\ok err inp -> ok inp err inp)

{-# INLINE putInput #-}
putInput :: a -> Parsec a ()
putInput inp = parsec (\ok err _ -> ok () err inp)

{-# INLINE parseError #-}
parseError :: c -> Parsec a b
parseError e = parsec (\ok err inp -> err)

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\ok -> ok x)

{-# INLINE parsecBind #-}
x `parsecBind` f = parsec (\ok -> runParsec x (\y -> runParsec (f y) ok))

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 = parsec (\ok err inp ->
  runParsec m1 ok (runParsec m2 ok err inp) inp)

{-# INLINE (<+>) #-}
m1 <+> m2 = cut' (m1 `parsecChoice` m2)

run :: Stream a => Parsec a b -> a -> Result b
run p x = runParsec p ok err x
  where ok x _ _ = Ok x
        err = Error

{-# INLINE cut #-}
cut :: Stream a => Parsec a ()
cut = parsec (\ok err inp -> ok () Error inp)

{-# INLINE cut' #-}
cut' :: Stream a => Parsec a b -> Parsec a b
cut' p = parsec (\ok err -> runParsec p (\x _ inp' -> ok x err inp') err)

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a b -> String -> Parsec a b
p <?> text = p

checkpoint = return ()
progress = return ()
success = id

peek = fmap hd getInput