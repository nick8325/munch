{-# LANGUAGE Rank2Types, TypeFamilies, CPP, NoMonomorphismRestriction #-}
module Prim where

import Base
import Control.Applicative
import Control.Monad
import Data.List

-- Parser type and monad instances

newtype Parsec a b = Parsec (forall c. Inner a b c)

type Inner a b c =
                 (b -> Reply a c -> a -> Reply a c) -- ok: success
              -> Reply a c                          -- err: backtracking failure
              -> a -> Reply a c

{-# INLINE eta #-}
eta :: Inner a b c -> Inner a b c
eta p = \ok err inp here there -> p ok err inp here there

{-# INLINE parsec #-}
parsec :: (forall c. Inner a b c) -> Parsec a b
parsec p = Parsec (eta p)

{-# INLINE runParsec #-}
runParsec :: Parsec a b -> Inner a b c
runParsec (Parsec p) = eta p

type Reply a b = Failed a -> Maybe Error -> Maybe Error -> Result a b
data Failed a = Nil | Cons_ a (Maybe Error) (Failed a) deriving Show

data Result a b = Ok b | Error (Failed a) (Maybe Error) (Maybe Error) deriving Show
data Error = Expected String | Message String deriving Show

errorMsg = Message
expectedMsg = Expected

{-# INLINE getInput #-}
getInput :: Parsec a a
getInput = parsec (\ok err inp -> ok inp err inp)

{-# INLINE putInput #-}
putInput :: a -> Parsec a ()
putInput inp = parsec (\ok err _ -> ok () err inp)

{-# INLINE parseError #-}
parseError :: [Error] -> Parsec a b
parseError [] = parseError' Nothing
parseError (e:_) = parseError' (Just e)

{-# INLINE parseError' #-}
parseError' :: Maybe Error -> Parsec a b
parseError' e = parsec (\ok err inp failed here there -> err (Cons_ inp here failed) e e)

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a b -> String -> Parsec a b
#ifndef IGNORE_LABELS
p <?> text = parsec (\ok err inp failed here there ->                      
  runParsec p ok err inp failed e e)
  where e = Just (Expected text)
#else
p <?> text = p
#endif

{-# INLINE parsecReturn #-}
parsecReturn x = parsec (\ok -> ok x)

{-# INLINE parsecBind #-}
x `parsecBind` f = parsec (\ok -> runParsec x (\y -> runParsec (f y) ok))

{-# INLINE parsecChoice #-}
m1 `parsecChoice` m2 = parsec (\ok err inp ->
  runParsec m1 ok (runParsec m2 ok err inp) inp)

run :: Stream a => Parsec a b -> a -> Result a b
run p x = runParsec p ok err x Nil Nothing Nothing
  where ok x _ _ _ _ _ = Ok x
        err = Error

{-# INLINE cut #-}
cut :: Stream a => Parsec a ()
cut = parsec (\ok err inp failed here there -> ok () Error inp Nil here there)

{-# INLINE cut' #-}
cut' :: Stream a => Parsec a b -> Parsec a b
cut' p = parsec (\ok err inp failed -> runParsec p (\x _ inp' _ here there -> ok x err inp' failed here there) err inp failed)

checkpoint = return ()
progress = parsec (\ok err inp failed here there -> ok () err inp failed Nothing there)