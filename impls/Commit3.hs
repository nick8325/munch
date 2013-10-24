{-# LANGUAGE Rank2Types, TypeFamilies, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List
import GHC.Exts
import Prelude hiding (abs)

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Token a -> One b (Inner a b c))

type Inner a b c =
                 (b -> Fun a (Reply c)) -- ok: success
              -> Reply c                -- err: backtracking failure
              -> Fun a (Reply c)

{-# INLINE eta #-}
eta :: Stream a => Inner a b c -> Inner a b c
eta p = \ok err -> abs (\inp -> app (p ok err) inp)

{-# INLINE case_ #-}
case_ :: Stream a => One b (Inner a b c) -> One b (Inner a b c)
case_ x =
  case x of
    Passive x -> Passive x
    Block f -> Block (eta f)
    Err -> Err

{-# INLINE parsec #-}
parsec :: Stream a => (forall c. Token a -> One b (Inner a b c)) -> Parsec a b
parsec p = Parsec (\x -> case_ (p x))

{-# INLINE runParsec #-}
runParsec :: Stream a => Parsec a b -> Token a -> One b (Inner a b c)
runParsec (Parsec p) = \x -> case_ (p x)
  -- \ok err -> abs (\inp ->
  --   case l (hd inp) of
  --     Err -> err
  --     _ -> app (p ok err) inp)

type Reply a = Result a

data Result a = Ok a | Error deriving Show

data One a b = Passive a | Block b | Err

errorMsg = undefined
expectedMsg = undefined

{-# INLINE getInput #-}
getInput :: Stream a => Parsec a a
getInput = parsec (\_ -> Block (\ok err -> abs (\inp -> app (ok inp) inp)))

{-# INLINE putInput #-}
putInput :: Stream a => a -> Parsec a ()
putInput inp = parsec (\_ -> Block (\ok err -> abs (\_ -> app (ok ()) inp)))

{-# INLINE parseError #-}
parseError :: Stream a => c -> Parsec a b
parseError e = parsec (\_ -> Err)

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\_ -> Passive x)

{-# INLINE inner #-}
inner :: Stream a => One b (Inner a b c) -> Inner a b c
inner (Passive x) = \ok err -> ok x
inner Err = \ok err -> abs (\_ -> err)
inner (Block p) = p

{-# INLINE parsecBind #-}
x `parsecBind` f =
  parsec (\t ->
    case runParsec x t of
      Passive y -> runParsec (f y) t
      Err -> Err
      Block p ->
        Block (\ok err -> p (\y -> abs (\inp -> inner (runParsec (f y) (hd inp)) ok err `app` inp)) err))

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 =
  parsec (\t ->
    case (runParsec m1 t, runParsec m2 t) of           
      (Err, x) -> x
      (Passive y, _) -> Passive y
      (x, Err) -> x
      (Block x, Passive y) -> Block (\ok err -> abs (\inp -> x ok (ok y `app` inp) `app` inp))
      (Block x, Block y) -> Block (\ok err -> abs (\inp -> x ok (y ok err `app` inp) `app` inp)))

run :: Stream a => Parsec a b -> a -> Result b
run p x = inner (runParsec p (hd x)) ok err `app` x
  where ok x = abs (\_ -> Ok x)
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

peek :: Stream a => Parsec a (Token a)
peek = parsec Passive

success = id