{-# LANGUAGE Rank2Types, TypeFamilies #-}
module Common where

import Control.Applicative
import Control.Monad
import Base
import Prim

{-# INLINE fatalError #-}
fatalError :: Stream a => String -> Parsec a b
fatalError e = cut *> parseError [errorMsg e]

instance Stream a => Monad (Parsec a) where
  {-# INLINE return #-}
  return = parsecReturn
  {-# INLINE (>>=) #-}
  (>>=) = parsecBind
  {-# INLINE fail #-}
  fail _ = parseError []

instance Stream a => MonadPlus (Parsec a) where
  {-# INLINE mzero #-}
  mzero = parseError []
  {-# INLINE mplus #-}
  mplus = parsecChoice

{-# INLINE eof #-}
eof :: Stream a => Parsec a ()
eof = do
  inp <- getInput
  primToken inp
    (\_ _ -> parseError [expectedMsg "end of file"])
    (return ())
    fatalError

-- User state

data UserState state stream = UserState { userState :: !state, userStream :: !stream }

instance Stream a => Stream (UserState state a) where
  type Token (UserState state a) = Token a
  {-# INLINE primToken #-}
  primToken (UserState state stream) ok err =
    primToken stream (ok . UserState state) err

{-# INLINE getState #-}
getState :: Stream a => Parsec (UserState state a) state
getState = fmap userState getInput

{-# INLINE putState #-}
putState :: Stream a => state -> Parsec (UserState state a) ()
putState state = do
  input <- getInput
  putInput input { userState = state }

instance Stream a => Functor (Parsec a) where
  {-# INLINE fmap #-}
  fmap f x = x >>= return . f

instance Stream a => Applicative (Parsec a) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  f <*> x = do { f' <- f; x' <- x; return (f' x') }
  {-# INLINE (*>) #-}
  (*>) = (>>)
  {-# INLINE (<*) #-}
  x <* y = do
    x' <- x
    y
    return x'

instance Stream a => Alternative (Parsec a) where
  {-# INLINE empty #-}
  empty = mzero
  {-# INLINE (<|>) #-}
  (<|>) = mplus
  {-# INLINE some #-}
  some p = do { x <- p; xs <- many p; return (x:xs) }
  {-# INLINE many #-}
  many p = p' where p' = liftM2 (:) p p' <|> return []
  -- Stack overflow-avoiding version:
  -- many p = liftM reverse (p' [])
  --   where p' !xs = do { x <- nonempty p; p' (x:xs) } `mplus` return xs

{-# INLINE skipSome #-}
skipSome :: Stream a => Parsec a b -> Parsec a ()
skipSome p = p' where p' = p >> (p' `mplus` return ())

{-# INLINE skipMany #-}
skipMany :: Stream a => Parsec a b -> Parsec a ()
skipMany p = p' where p' = (p >> p') `mplus` return ()

{-# INLINE between #-}
between :: Stream a => Parsec a b -> Parsec a c -> Parsec a d -> Parsec a d
between p q r = p *> r <* q

{-# INLINE sepBy1 #-}
sepBy1 :: Stream a => Parsec a b -> Parsec a c -> Parsec a [b]
sepBy1 it sep = liftM2 (:) it (many (sep >> it))

{-# INLINE next #-}
next :: Stream a => Parsec a (Token a)
next = do
  inp <- getInput
  primToken inp
    (\inp' x -> do { putInput inp'; progress; return x })
    mzero
    fatalError

{-# INLINE satisfy #-}
satisfy :: Stream a => (Token a -> Bool) -> Parsec a (Token a)
satisfy p = do
  t <- next
  guard (p t)
  -- cut
  checkpoint
  return t

{-# INLINE char #-}
char :: (Stream a, Token a ~ Char) => Char -> Parsec a Char
char x = satisfy (== x) <?> show x

{-# INLINE prim #-}
prim :: Stream i => (forall b. i -> (a -> i -> b) -> b -> b) -> Parsec i a
prim f = do
  inp <- getInput
  f inp (\x inp' -> do { putInput inp'; return x }) mzero

{-# INLINE skip #-}
skip :: Stream i => (i -> i) -> Parsec i ()
skip f = do
  inp <- getInput
  putInput (f inp)
