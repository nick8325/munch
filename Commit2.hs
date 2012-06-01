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
                           one :: Token a -> One b,  
                           zero :: One b }

type Inner a b c =
                 (b -> Fun a (Reply c)) -- ok: success
              -> Reply c                -- err: backtracking failure
              -> Fun a (Reply c)

{-# INLINE eta #-}
eta :: Stream a => Inner a b c -> Inner a b c
eta p = \ok err -> abs (\inp -> app (p ok err) inp)

{-# INLINE case_ #-}
case_ :: One a -> One a
case_ x = \p y b e -> x p y b e

{-# INLINE parsec #-}
parsec :: Stream a => (forall c. Inner a b c) -> (Token a -> One b) -> One b -> Parsec a b
parsec p l z = Parsec (eta p) (\x -> case_ (l x)) (case_ z)

{-# INLINE runParsec #-}
runParsec :: Stream a => Parsec a b -> Inner a b c
runParsec (Parsec p _ _) = eta p

type Reply a = Result a

data Result a = Ok a | Error deriving Show

type One a = forall b.
     (a -> b) -- passive
  -> (a -> b) -- yum
  -> b        -- block
  -> b        -- err
  -> b
{-# INLINE passive #-}
{-# INLINE yum #-}
passive, yum :: a -> One a
passive x = \p _ _ _ -> p x
yum x = \_ y _ _ -> y x
{-# INLINE block #-}
block = \_ _ b _ -> b
{-# INLINE err #-}
err = \_ _ _ e -> e

errorMsg = undefined
expectedMsg = undefined

{-# INLINE getInput #-}
getInput :: Stream a => Parsec a a
getInput = parsec (\ok err -> abs (\inp -> app (ok inp) inp)) (\_ -> block) block

{-# INLINE putInput #-}
putInput :: Stream a => a -> Parsec a ()
putInput inp = parsec (\ok err -> abs (\_ -> app (ok ()) inp)) (\_ -> block) block

{-# INLINE parseError #-}
parseError :: Stream a => c -> Parsec a b
parseError e = parsec (\ok err -> abs (\inp -> err)) (\_ -> err) err

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\ok err -> ok x) (\_ -> passive x) (passive x)

{-# INLINE parsecBind #-}
x `parsecBind` f =
  parsec (\ok err -> runParsec x (\y -> runParsec (f y) ok err) err)
         (\t p y b e ->
           one x t
             (\x -> one (f x) t p y b e)
             (\x -> zero (f x) y y b e)
             b e)
         (\p y b e ->
           zero x (\x -> zero (f x) p y b e)
                  (\x -> error "oops") b e)

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 =
  parsec (\ok err -> abs (\inp ->
           one m2 (hd inp)
             (\_ -> runParsec m1 ok (runParsec m2 ok err `app` inp) `app` inp)
             (\_ -> runParsec m1 ok (runParsec m2 ok err `app` inp) `app` inp)
             (runParsec m1 ok (runParsec m2 ok err `app` inp) `app` inp)
             (runParsec m1 ok err `app` inp)))
         (\t p y b e ->
           one m1 t p y b (one m2 t p y b e))
         (\p y b e ->
           zero m1 p (error "oops") b (zero m2 p y b e))

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
success p = parsec (\ok err -> abs (\inp -> runParsec p ok Error `app` inp)) (one p) (zero p)

peek :: Stream a => Parsec a (Token a)
peek = parsec (\ok err -> abs (\inp -> ok (hd inp) `app` inp)) passive block
