{-# LANGUAGE Rank2Types, TypeFamilies, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Inner a b c)

type Inner a b c =
                 (b -> Token a -> a -> Reply c) -- ok: success
              -> Reply c                        -- err: backtracking failure
              -> Token a -> a -> Reply c

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \ok err hd inp -> p ok err hd inp

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
getInput = parsec (\ok err hd inp -> ok inp hd inp)

{-# INLINE putInput #-}
putInput :: Stream a => a -> Parsec a ()
putInput inp = parsec (\ok err _ _ -> ok () (hd inp) inp)

{-# INLINE parseError #-}
parseError :: c -> Parsec a b
parseError e = parsec (\ok err hd inp -> err)

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\ok err -> ok x)

{-# INLINE parsecBind #-}
x `parsecBind` f = parsec (\ok err -> runParsec x (\y -> runParsec (f y) ok err) err)

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 = parsec (\ok err hd inp ->
  runParsec m1 ok (runParsec m2 ok err hd inp) hd inp)

run :: Stream a => Parsec a b -> a -> Result b
run p x = runParsec p ok err (hd x) x
  where ok x _ _ = Ok x
        err = Error

{-# INLINE cut #-}
cut :: Stream a => Parsec a ()
cut = parsecReturn ()

{-# INLINE cut' #-}
cut' :: Stream a => Parsec a b -> Parsec a b
cut' = id

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a b -> String -> Parsec a b
p <?> text = p

checkpoint = return ()
progress = return ()

{-# INLINE success #-}
success :: Parsec a b -> Parsec a b
success p = parsec (\ok err inp -> runParsec p ok Error inp)

peek :: Parsec a (Token a)
peek = parsec (\ok err hd inp -> ok hd hd inp)