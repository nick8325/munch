{-# LANGUAGE Rank2Types, TypeFamilies, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Inner a b c)

data Inner a b c = Inner (Char -> (b -> c) -> Maybe c) (Maybe b)

type Inner a b c = ((a -> Maybe c) -> Maybe b -> c) -> c

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \k -> p (\n e -> k n e)

{-# INLINE parsec #-}
parsec :: (forall c. Inner a b c) -> Parsec a b
parsec p = Parsec (eta p)

{-# INLINE runParsec #-}
runParsec :: Parsec a b -> Inner a b c
runParsec (Parsec p) = eta p

type Reply a = Result a

data Result a = Ok a | Error deriving (Eq, Ord, Show)

errorMsg = undefined
expectedMsg = undefined

{-# INLINE parseError #-}
parseError :: c -> Parsec a b
parseError e = parsec (\k -> k (\_ -> Nothing) Nothing)

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\k -> k (\_ -> Nothing) (Just x))

{-# INLINE parsecBind #-}
x `parsecBind` f =
  parsec (\k ->
    x (\n e ->
        case e of
          Nothing -> k (\c -> n c 

{-
{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 = parsec (\ok err inp ->
  runParsec m1 ok (runParsec m2 ok err inp) inp)

run :: Stream a => Parsec a b -> a -> Result b
run p x = runParsec p ok err x
  where ok x _ = Ok x
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

peek :: Stream a => Parsec a (Token a)
peek = parsec (\ok err inp -> ok (hd inp) inp)
-}