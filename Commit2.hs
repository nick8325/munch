{-# LANGUAGE Rank2Types, TypeFamilies, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List
import GHC.Exts
import Prelude hiding (abs)

-- Parser type and monad instances

newtype Parsec a b = Parsec { unParsec :: forall c. Token a -> One a b c }

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
parsec :: Stream a => (forall c. Token a -> One a b c) -> Parsec a b
parsec p = Parsec (\t -> case_ (p t))

{-# INLINE runParsec #-}
runParsec :: Stream a => Parsec a b -> Token a -> One a b c
runParsec (Parsec p) = \t -> case_ (p t)

type Reply a = Result a

data Result a = Ok a | Error deriving Show

data One a b c = Passive (Result b) | Block (Inner a b c)

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
parseError e = parsec (\_ -> Passive Error)

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\_ -> Passive (Ok x))

{-# INLINE activate #-}
activate x =
  \ok err -> abs (\inp -> case runParsec x (hd inp) of
                     Passive (Ok x) -> ok x `app` inp
                     Passive Error -> err
                     Block p -> p ok err `app` inp)

{-# INLINE parsecBind #-}
x `parsecBind` f =
  parsec (\t ->
           case runParsec x t of
             Passive (Ok x) -> runParsec (f x) t
             Passive Error -> Passive Error
             Block p -> Block (\ok err -> abs (\inp -> p (\y -> activate (f y) ok err) err `app` inp)))


{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 =
  parsec (\t ->
           case (runParsec m1 t, runParsec m2 t) of
             (Passive (Ok x), _) -> Passive (Ok x)
             (Passive Error, x) -> x
             (x@Block{}, Passive Error) -> x
             (Block p, Passive (Ok x)) -> Block (\ok err -> abs (\inp -> p ok (ok x `app` inp) `app` inp))
             (Block p, Block q) -> Block (\ok err -> abs (\inp -> p ok (q ok err `app` inp) `app` inp)))

run :: Stream a => Parsec a b -> a -> Result b
run p x = activate p ok err `app` x
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
success x = parsec (\t ->
                     case runParsec x t of
                       Block p -> Block (\ok err -> abs (\inp -> p ok Error `app` inp))
                       y -> y)

peek :: Stream a => Parsec a (Token a)
peek = parsec (\t -> Passive (Ok t))
