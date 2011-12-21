{-# LANGUAGE RankNTypes, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TypeFamilies #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec
  { runParsec :: forall c.
                 (b -> Reply c -> a -> Reply c) -- ok: success
              -> Reply c                        -- err: backtracking failure
              -> a -> Reply c }

type Reply a = [Error] -> Result a

data Result a = Ok a | Error [Error]
data Error = Expected String | Message String

errorMsg = Message
expectedMsg = Expected

{-# INLINE getInput #-}
getInput :: Parsec a a
getInput = Parsec (\ok err inp exp -> ok inp err inp exp)

{-# INLINE putInput #-}
putInput :: a -> Parsec a ()
putInput inp = Parsec (\ok err _ exp -> ok () err inp exp)

{-# INLINE parseError #-}
parseError :: [Error] -> Parsec a b
parseError e = Parsec (\ok err inp exp -> err (e ++ exp))

{-# INLINE parsecReturn #-}
parsecReturn x = Parsec (\ok err inp exp -> ok x err inp exp)

{-# INLINE parsecBind #-}
x `parsecBind` f = Parsec (\ok err inp exp -> runParsec x (\y err inp exp -> runParsec (f y) ok err inp exp) err inp exp)

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 = Parsec (\ok err inp exp ->
  runParsec m1 ok (\exp -> runParsec m2 ok err inp exp) inp exp)

run :: Stream a => Parsec a b -> a -> Result b
run p x = runParsec p ok err x []
  where ok x _ _ _ = Ok x
        err = Error

{-# INLINE cut #-}
cut :: Stream a => Parsec a ()
cut = Parsec (\ok err inp exp -> ok () Error inp [])

{-# INLINE cut' #-}
cut' :: Stream a => Parsec a b -> Parsec a b
cut' p = Parsec (\ok err inp exp -> runParsec p (\x _ inp' _ -> ok x err inp' []) err inp exp)
