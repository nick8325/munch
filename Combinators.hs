{-# LANGUAGE Rank2Types, TypeFamilies #-}
module Combinators where

import Control.Applicative
import Control.Monad
import Class
import Stream

{-# INLINE eof #-}
eof :: Parser p => p ()
eof = do
  Nothing <- peek
  return ()

{-# INLINE skipSome #-}
skipSome :: Parser p => p a -> p ()
skipSome p = p' where p' = progress p >> success (p' `mplus` return ())

{-# INLINE skipMany #-}
skipMany :: Parser p => p a -> p ()
skipMany p = p' where p' = success $ (progress p >> p') `mplus` return ()

{-# INLINE between #-}
between :: Parser p => p a -> p b -> p c -> p c
between p q r = p *> r <* q

{-# INLINE sepBy1 #-}
sepBy1 :: Parser p => p a -> p b -> p [a]
sepBy1 it sep = liftM2 (:) it (many (sep >> it))

{-# INLINE next #-}
next :: Parser p => p ()
next = do
  Just{} <- peek
  Just (_, inp) <- fmap uncons getInput
  putInput inp

{-# INLINE satisfy #-}
satisfy :: Parser p => (Token (StreamType p) -> Bool) -> p (Token (StreamType p))
satisfy p = do
  Just x <- peek
  guard (p x)
  Just (_, inp) <- fmap uncons getInput
  putInput inp
  return x

{-# INLINE char #-}
char :: (Parser p, Token (StreamType p) ~ Char) => Char -> p Char
char x = satisfy (== x) <?> show x