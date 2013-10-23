{-# LANGUAGE Rank2Types, TypeFamilies, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List
import GHC.Exts
import Prelude hiding (abs)

-- Parser type and monad instances

data Parsec a b = Parsec { parser :: forall c. Inner a b c,
                           one :: Token a -> One b }

type Inner a b c =
                 (b -> Fun a (Reply c)) -- ok: success
              -> Reply c                -- err: backtracking failure
              -> Fun a (Reply c)

{-# INLINE eta #-}
eta :: Stream a => Inner a b c -> Inner a b c
eta p = \ok err -> abs (\inp -> app (p ok err) inp)

{-# INLINE case_ #-}
case_ :: One a -> One a
case_ x =
  case x of
    Passive x -> Passive x
    Block -> Block
    Err -> Err

{-# INLINE parsec #-}
parsec :: Stream a => (forall c. Inner a b c) -> (Token a -> One b) -> Parsec a b
parsec p l = Parsec (eta p) (\x -> case_ (l x))

{-# INLINE runParsec #-}
runParsec :: Stream a => Parsec a b -> Inner a b c
runParsec (Parsec p l) = eta p
  -- \ok err -> abs (\inp ->
  --   case l (hd inp) of
  --     Err -> err
  --     _ -> app (p ok err) inp)

type Reply a = Result a

data Result a = Ok a | Error deriving Show

data One a = Passive a | Block | Err

errorMsg = undefined
expectedMsg = undefined

{-# INLINE getInput #-}
getInput :: Stream a => Parsec a a
getInput = parsec (\ok err -> abs (\inp -> app (ok inp) inp)) (\_ -> Block)

{-# INLINE putInput #-}
putInput :: Stream a => a -> Parsec a ()
putInput inp = parsec (\ok err -> abs (\_ -> app (ok ()) inp)) (\_ -> Block)

{-# INLINE parseError #-}
parseError :: Stream a => c -> Parsec a b
parseError e = parsec (\ok err -> abs (\inp -> err)) (\_ -> Err)

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\ok err -> ok x) (\_ -> Passive x)

{-# INLINE parsecBind #-}
x `parsecBind` f =
  parsec (\ok err -> abs (\inp -> runParsec x (\y -> runParsec (f y) ok err) err `app` inp))
         (\t ->
           case one x t of
             Passive x -> one (f x) t
             Block -> Block
             Err -> Err)

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 =
  parsec (\ok err -> abs (\inp ->
           case (one m1 (hd inp), one m2 (hd inp)) of
             (Err, _) -> runParsec m2 ok err `app` inp
             (_, Err) -> runParsec m1 ok err `app` inp
             -- Err -> runParsec m1 ok err `app` inp
             _ -> runParsec m1 ok (runParsec m2 ok err `app` inp) `app` inp))
         (\t ->
           case one m1 t of
             Err -> one m2 t
             x -> x)

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
success p = parsec (\ok err -> abs (\inp -> runParsec p ok Error `app` inp)) (one p)

peek :: Stream a => Parsec a (Token a)
peek = parsec (\ok err -> abs (\inp -> ok (hd inp) `app` inp)) Passive
