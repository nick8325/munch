{-# LANGUAGE RankNTypes, BangPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TypeFamilies #-}
module Parsec where

import Control.Applicative
import Control.Monad
import Data.List

-- TO DO: longest prefix error messages (do we need to eagerly prune?)
-- TO DO: partial input with help of "want more starting from" return value, special combinator to say "restart from here"
-- (i.e. make restart explicit. Don't put it everywhere because it'll make too much stuff recursive and duplicate code---
-- attoparsec gets around the recursive thing by disallowing empty bytestrings so you have to make progress)

-- Parser type and monad instances

newtype Parsec a b = Parsec
  { runParsec :: forall c.
                 (b -> (Result a c -> Reply a c) -> Result a c -> a -> Reply a c) -- ok: success
              -> (Result a c -> Reply a c) -- err: backtracking failure
              -> Result a c -- more: partial input
              -> a -> Reply a c }

type Reply a b = [String] -> Result a b

data Result a b = Ok a b | Error a String | Expected a [String] | More (a -> Result a b)


checkpoint :: Parsec a ()
checkpoint = Parsec (\ok err more inp exp ->
  let x inp = ok () err (More x) inp exp
  in x inp)

{-# INLINE parseError #-}
parseError :: [String] -> Parsec a b
parseError e = Parsec (\ok err more inp exp -> err more (e ++ exp))

{-# INLINE fatalError #-}
fatalError :: Stream a => String -> Parsec a b
fatalError e = Parsec (\ok err more inp _ -> Error inp e)

instance Functor (Parsec a) where
  {-# INLINE fmap #-}
  fmap f x = x >>= return . f

instance Monad (Parsec a) where
  {-# INLINE return #-}
  return x = Parsec (\ok err more inp exp -> ok x err more inp exp)
  {-# INLINE (>>=) #-}
  x >>= f = Parsec (\ok err more inp exp -> runParsec x (\y err more inp exp -> runParsec (f y) ok err more inp exp) err more inp exp)
  {-# INLINE fail #-}
  fail _ = parseError []

instance MonadPlus (Parsec a) where
  {-# INLINE mzero #-}
  mzero = Parsec (\ok err more inp exp -> err more exp)
  {-# INLINE mplus #-}
  m1 `mplus` m2 = Parsec (\ok err more inp exp ->
    runParsec m1 ok (\more exp -> runParsec m2 ok err more inp exp) more inp exp)

instance Applicative (Parsec a) where
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

instance Alternative (Parsec a) where
  {-# INLINE empty #-}
  empty = mzero
  {-# INLINE (<|>) #-}
  (<|>) = mplus
  {-# INLINE some #-}
  some p = do { x <- nonempty p; xs <- many p; return (x:xs) }
  {-# INLINE many #-}
  many p = p' where p' = liftM2 (:) (nonempty p) p' <|> return []
  -- Stack overflow-avoiding version:
  -- many p = liftM reverse (p' [])
  --   where p' !xs = do { x <- nonempty p; p' (x:xs) } `mplus` return xs

-- Basic combinators

{-# INLINE nonempty #-}
nonempty :: Parsec a b -> Parsec a b
nonempty p = p

{-# INLINE skipSome #-}
skipSome :: Parsec a b -> Parsec a ()
skipSome p = p' where p' = nonempty p >> (p' `mplus` return ())

{-# INLINE skipMany #-}
skipMany :: Parsec a b -> Parsec a ()
skipMany p = p' where p' = (nonempty p >> p') `mplus` return ()

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a b -> String -> Parsec a b
p <?> text = Parsec (\ok err more inp exp ->
  runParsec p ok err more inp (text:exp))

{-# INLINE between #-}
between :: Parsec a b -> Parsec a c -> Parsec a d -> Parsec a d
between p q r = p *> r <* q

{-# INLINE sepBy1 #-}
sepBy1 :: Parsec a b -> Parsec a c -> Parsec a [b]
sepBy1 it sep = liftM2 (:) it (many (sep >> it))

-- Running the parser

run_ :: Stream a => Parsec a b -> a -> Result a b
run_ p x = runParsec p ok err more x []
  where ok x _ _ inp _ = Ok inp x
        err _ exp = Expected x (reverse exp)
        more = More (run_ p)

run :: Stream a => (a -> [String]) -> Parsec a b -> a -> (a, Either [String] b)
run report p ts =
  case run_ p ts of
    Ok ts' x -> (ts', Right x)
    Error ts' e -> (ts', Left [e])
    Expected ts' e -> (ts', Left (expected (report ts') e))

-- Reporting errors

expected :: [String] -> [String] -> [String]
expected unexpected [] = unexpected ++ ["Unknown error"]
expected unexpected expected =
  unexpected ++ [ "Expected " ++ list expected ]
  where list [exp] = exp
        list exp = intercalate ", " (init exp) ++ " or " ++ last exp

-- Token streams

class Stream a where
  type Token a
  primToken :: a -> (a -> Token a -> b) -> b -> (String -> b) -> b

{-# INLINE next #-}
next :: Stream a => Parsec a (Token a)
next = Parsec (\ok err more inp exp ->
  primToken inp (\inp' x -> ok x err more inp' exp) (err more exp) (Error inp))

{-# INLINE cut #-}
cut :: Stream a => Parsec a ()
cut = Parsec (\ok err more inp exp -> ok () (const (Expected inp)) more inp [])

{-# INLINE cut' #-}
cut' :: Stream a => Parsec a b -> Parsec a b
cut' p = Parsec (\ok err more inp exp -> runParsec p (\x _ more inp' _ -> ok x err more inp' []) err more inp exp)

{-# INLINE satisfy #-}
satisfy :: Stream a => (Token a -> Bool) -> Parsec a (Token a)
satisfy p = do
  t <- next
  guard (p t)
  cut
  return t

{-# INLINE eof #-}
eof :: Stream a => Parsec a ()
eof = Parsec (\ok err more inp exp ->
  primToken inp (\_ _ -> more) (ok () err more inp exp) (Error inp))

-- User state

data UserState state stream = UserState { userState :: !state, userStream :: !stream }

instance Stream a => Stream (UserState state a) where
  type Token (UserState state a) = Token a
  {-# INLINE primToken #-}
  primToken (UserState state stream) ok err =
    primToken stream (ok . UserState state) err

{-# INLINE getState #-}
getState :: Parsec (UserState state a) state
getState = Parsec (\ok err more inp@UserState{userState = state} exp -> ok state err more inp exp)

{-# INLINE putState #-}
putState :: state -> Parsec (UserState state a) ()
putState state = Parsec (\ok err more inp@UserState{userStream = stream} exp -> ok () err more (UserState state stream) exp)