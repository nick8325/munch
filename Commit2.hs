{-# LANGUAGE Rank2Types, TypeFamilies, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List
import GHC.Exts
import Prelude hiding (abs)

-- Parser type and monad instances

data Parsec a b = Parsec {
  here :: forall c. Inner a b c,
  feed :: forall c. Token a -> One a b c
  }

type Inner a b c =
                 (b -> Fun a (Reply c)) -- ok: success
              -> Reply c                -- err: backtracking failure
              -> Fun a (Reply c)

{-# INLINE eta #-}
eta :: Stream a => Inner a b c -> Inner a b c
eta p = \ok err -> abs (\inp -> app (p ok err) inp)

{-# INLINE case_ #-}
case_ :: Stream a => One a b c -> One a b c
case_ x =
  case x of
    Passive (Ok x) -> Passive (Ok x)
    Passive Error -> Passive Error
    Block p -> Block (eta p)

{-# INLINE parsec #-}
parsec :: Stream a => (forall c. Inner a b c) -> (forall c. Token a -> One a b c) -> Parsec a b
parsec p q = Parsec (eta p) (\t -> case_ (q t))

{-# INLINE greedy #-}
greedy :: Stream a => (forall c. Inner a b c) -> Parsec a b
greedy p = parsec p (\_ -> Block p)

{-# INLINE runParsec #-}
runParsec :: Stream a => Parsec a b -> Inner a b c
runParsec (Parsec p _) = eta p

{-# INLINE feedParsec #-}
feedParsec :: Stream a => Parsec a b -> Token a -> One a b c
feedParsec (Parsec _ p) = \t -> case_ (p t)

type Reply a = Result a

data Result a = Ok a | Error deriving Show

data One a b c = Passive (Result b) | Block (Inner a b c)

errorMsg = undefined
expectedMsg = undefined

{-# INLINE getInput #-}
getInput :: Stream a => Parsec a a
getInput = greedy (\ok err -> abs (\inp -> app (ok inp) inp))

{-# INLINE putInput #-}
putInput :: Stream a => a -> Parsec a ()
putInput inp = greedy (\ok err -> abs (\_ -> app (ok ()) inp))

{-# INLINE parseError #-}
parseError :: Stream a => c -> Parsec a b
parseError e = parsec (\ok err -> abs (\inp -> err)) (\_ -> Passive Error)

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\ok err -> abs (\inp -> ok x `app` inp)) (\_ -> Passive (Ok x))

{-# INLINE parsecBind #-}
x `parsecBind` f =
  parsec (\ok err -> abs (\inp -> runParsec x (\y -> runParsec (f y) ok err) err `app` inp))
         (\t ->
           case feedParsec x t of
             Passive (Ok x) -> feedParsec (f x) t
             Passive Error -> Passive Error
             Block p -> Block (\ok err -> abs (\inp -> p (\y -> runParsec (f y) ok err) err `app` inp)))

{-# INLINE activate #-}
activate x =
  \ok err -> abs (\inp -> case x (hd inp) of
                     Passive (Ok x) -> ok x `app` inp
                     Passive Error -> err
                     Block p -> p ok err `app` inp)

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 =
  parsec (activate (m1 `feedChoice` m2))
         (m1 `feedChoice` m2)

{-# INLINE feedChoice #-}
m1 `feedChoice` m2 = \t ->
  case (feedParsec m1 t, feedParsec m2 t) of
    (Passive (Ok x), _) -> Passive (Ok x)
    (Passive Error, x) -> x
    (x@Block{}, Passive Error) -> x
    (Block p, Passive (Ok x)) -> Block (\ok err -> abs (\inp -> p ok (ok x `app` inp) `app` inp))
    (Block p, Block q) -> Block (\ok err -> abs (\inp -> p ok (q ok err `app` inp) `app` inp))

run :: Stream a => Parsec a b -> a -> Result b
run p x = runParsec p ok err `app` x
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

{-# INLINE success #-}
success :: Stream a => Parsec a b -> Parsec a b
success x = parsec (\ok err -> abs (\inp -> runParsec x ok Error `app` inp))
                   (\t ->
                     case feedParsec x t of
                       Block p -> Block (\ok err -> abs (\inp -> p ok Error `app` inp))
                       y -> y)

peek :: Stream a => Parsec a (Token a)
peek = parsec (\ok err -> abs (\inp -> ok (hd inp) `app` inp))
              (\t -> Passive (Ok t))