{-# LANGUAGE Rank2Types, TypeFamilies, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Inner a b c)

type Inner a b c = a -> Mybe ((a -> b -> Reply c) -> Reply c)

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \inp ok err _ -> p inp (\k -> ok (\ok' -> k (\inp' x -> ok' inp' x))) (\_ -> err ()) ()

{-# INLINE parsec #-}
parsec :: (forall c. Inner a b c) -> Parsec a b
parsec p = Parsec (eta p)

{-# INLINE runParsec #-}
runParsec :: Parsec a b -> Inner a b c
runParsec (Parsec p) = eta p

type Mybe a = forall b. (a -> Reply b) -> Reply b -> Reply b
type Reply a = () -> Result a

data Result a = Ok a | Error deriving (Eq, Ord, Show)

instance Monad Result where
  {-# INLINE return #-}
  return = Ok
  {-# INLINE (>>=) #-}
  Ok x >>= f = f x
  Error >>= f = Error

instance MonadPlus Result where
  {-# INLINE mzero #-}
  mzero = Error
  {-# INLINE mplus #-}
  Error `mplus` r = r
  r `mplus` _ = r

errorMsg = undefined
expectedMsg = undefined

{-# INLINE getInput #-}
getInput :: Parsec a a
getInput = parsec (\inp ok err -> ok (\k -> k inp inp))

{-# INLINE putInput #-}
putInput :: Stream a => a -> Parsec a ()
putInput inp = parsec (\_ ok _ -> ok (\k -> k inp ()))

{-# INLINE parseError #-}
parseError :: c -> Parsec a b
parseError e = parsec (\inp _ err -> err)

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\inp ok _ -> ok (\k -> k inp x))

{-# INLINE parsecBind #-}
x `parsecBind` f =
  parsec (\inp ok err ->
    runParsec x inp (\k -> k (\inp' y -> runParsec (f y) inp' ok err)) err)

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 =
  parsec (\inp ok err ->
    runParsec m1 inp ok (runParsec m2 inp ok err))                              

run :: Stream a => Parsec a b -> a -> Result b
run p x = runParsec p x (\k -> k (\_ x () -> Ok x)) (\_ -> Error) ()

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

{-# INLINE progress #-}
progress = parsec (\inp ok err -> ok (\k -> k inp ()))

success = id
{-

{-# INLINE peek #-}

peek :: Stream a => Parsec a (Token a)
peek = parsec (\ok err inp -> ok (hd inp) inp)
-}