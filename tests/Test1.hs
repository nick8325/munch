{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module Test1 where

import Parsec

data Chars = Cons {-# UNPACK #-} !Char Chars

data Expr = Var Char | Fun Char [Expr] deriving Show

instance Stream [a] where
  type Token [a] = a
  primToken [] _ err _ = err
  primToken (x:xs) f _ _ = f xs x
  -- type Position [a] = ()
  -- position _ = ()

instance Stream Chars where
  type Token Chars = Char
  primToken (Cons c cs) f _ _ = f cs c
  -- type Position Chars = ()
  -- position _ = ()

fromStr :: String -> Chars
fromStr [] = undefined
fromStr (c:cs) = Cons c (fromStr cs)