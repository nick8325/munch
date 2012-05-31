{-# LANGUAGE Rank2Types, TypeFamilies, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List
import GHC.Exts

-- Parser type and monad instances

data Parsec a b = Parsec { parser :: forall c. Inner a b c,
                           one :: Token a -> One b,  
                           zero :: One b }

type Inner a b c =
                 (b -> Token a -> a -> Reply c) -- ok: success
              -> Reply c             -- err: backtracking failure
              -> Token a -> a -> Reply c

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \ok err hd inp -> p ok err hd inp

{-# INLINE parsec #-}
parsec :: (forall c. Inner a b c) -> (Token a -> One b) -> One b -> Parsec a b
parsec p l z = Parsec (eta p) l z

{-# INLINE runParsec #-}
runParsec :: Parsec a b -> Inner a b c
runParsec (Parsec p _ _) = eta p

type Reply a = Result a

data Result a = Ok a | Error deriving Show

data One a = Passive a | Yum a | Block | Err deriving Show

errorMsg = undefined
expectedMsg = undefined

{-# INLINE getInput #-}
getInput :: Parsec a a
getInput = parsec (\ok err hd inp -> ok inp hd inp) (\_ -> Block) Block

{-# INLINE putInput #-}
putInput :: Stream a => a -> Parsec a ()
putInput inp = parsec (\ok err _ _ -> ok () (hd inp) inp) (\_ -> Block) Block

{-# INLINE parseError #-}
parseError :: c -> Parsec a b
parseError e = parsec (\ok err _ inp -> err) (\_ -> Err) Err

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\ok err -> ok x) (\_ -> Passive x) (Passive x)

{-# INLINE parsecBind #-}
x `parsecBind` f =
  parsec (\ok err -> runParsec x (\y -> runParsec (f y) ok err) err)
         (\t ->
           case one x t of
             Passive x -> one (f x) t
             Yum x ->
               case zero (f x) of
                 Passive x -> Yum x
                 x -> x
             Block -> Block
             Err -> Err)
         (case zero x of
             Passive x -> zero (f x)
             Yum x -> error "oops"
             Block -> Block
             Err -> Err)

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 =
  parsec (\ok err hd ->
           case (inline one m1 hd, inline one m2 hd) of
             (Err, Err) -> \inp -> inline err
             (_, Err) -> \inp -> inline runParsec m1 ok err hd inp
             (Err, _) -> \inp -> inline runParsec m2 ok err hd inp
             _ -> \inp -> inline runParsec m1 ok (runParsec m2 ok err hd inp) hd inp)
         (\t ->
           case one m1 t of
             Passive x -> Passive x
             Yum x -> Yum x
             Block -> Block
             Err -> one m2 t)
         (case zero m1 of
             Passive x -> Passive x
             Yum x -> error "oops"
             Block -> Block
             Err -> zero m2)

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
success p = parsec (\ok err hd inp -> runParsec p ok Error hd inp) (one p) (zero p)

peek :: Stream a => Parsec a (Token a)
peek = parsec (\ok err hd inp -> ok hd hd inp) Passive Block
