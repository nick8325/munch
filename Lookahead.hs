{-# LANGUAGE RankNTypes #-}
module Lookahead(runBrackets) where

import Control.Applicative
import Control.Monad

newtype Parsing s a = Parsing { 
  unParsing :: forall b. s -> (s -> a -> b) -> b -> b
  }

{-# INLINE parsingReturn #-}
parsingReturn x = Parsing (\xs ok _ -> ok xs x)

{-# INLINE parsingBind #-}
Parsing p `parsingBind` f =
  Parsing (\xs ok err -> p xs (\ys x -> unParsing (f x) ys ok err) err)

{-# INLINE parsingZero #-}
parsingZero = Parsing (\_ _ err -> err)

{-# INLINE parsingPlus #-}
Parsing p `parsingPlus` Parsing q =
  Parsing (\xs ok err -> p xs ok (q xs ok err))

data Parser a =
    Pure (Parsing () a)
  | Peek (Parsing String a)
         (forall b. Char -> (Parsing String a -> b -> b) -> b -> b)
  | Wild (Parsing String a)

{-# INLINE parserReturn #-}
parserReturn x = Pure (parsingReturn x)

{-# INLINE parserBind #-}
Pure p `parserBind` f =
  unParsing p () (\_ x -> f x) parserZero
Peek p q `parserBind` f =
  Peek (p `parsingBind` (parsing . f))
       (\c op e -> q c (\q' e -> op (q' `parsingBind` (parsing . f)) e) e)
Wild p `parserBind` f =
  Wild (p `parsingBind` (parsing . f))

{-# INLINE parserZero #-}
parserZero = Pure parsingZero

{-# INLINE parsing #-}
parsing :: Parser a -> Parsing String a
parsing (Pure p) = sully p
parsing (Peek p q) =
  Parsing $ \xs ok err ->
    case xs of
      [] -> unParsing p xs ok err
      (x:xs) -> q x (\q' k -> unParsing q' xs ok k) err
parsing (Wild p) = p

{-# INLINE sully #-}
sully :: Parsing () a -> Parsing s a
sully (Parsing p) = Parsing (\xs ok err -> p () (\_ x -> ok xs x) err)

{-# INLINE sully' #-}
sully' :: Parsing () a -> Char -> (Parsing String a -> b -> b) -> b -> b
sully' (Parsing p) c ok err = p () (\_ x -> ok (go x) err) err
  where
    {-# INLINE go #-}
    go x = Parsing (\xs ok err -> ok (c:xs) x)

{-# INLINE parserPlus #-}
Pure p `parserPlus` Pure q = Pure (p `parsingPlus` q)
Peek p p' `parserPlus` Pure q = Peek (p `parsingPlus` sully q) (\c ok err -> p' c ok (sully' q c ok err))
Pure p `parserPlus` Peek q q' = Peek (sully p `parsingPlus` q) (\c ok err -> sully' p c ok (q' c ok err))
Peek p p' `parserPlus` Peek q q' = Peek (p `parsingPlus` q) (\c ok err -> p' c ok (q' c ok err))

instance Monad Parser where
  {-# INLINE return #-}
  return = parserReturn
  
  {-# INLINE (>>=) #-}
  (>>=) = parserBind

instance MonadPlus Parser where
  {-# INLINE mzero #-}
  mzero = parserZero
  {-# INLINE mplus #-}
  mplus = parserPlus

instance Functor Parser where
  {-# INLINE fmap #-}
  fmap f m = do { x <- m; return (f x) }

instance Applicative Parser where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  mf <*> mx = do { f <- mf; x <- mx; return (f x) }
  {-# INLINE (*>) #-}
  (*>) = (>>)
  {-# INLINE (<*) #-}
  mx <* my = do { x <- mx; my; return x }

instance Alternative Parser where
  {-# INLINE empty #-}
  empty = mzero
  {-# INLINE (<|>) #-}
  (<|>) = mplus

{-# INLINE next #-}
next :: Parser Char
next = Peek (Parsing p) (\c ok err -> ok (parsingReturn c) err)
  where
    {-# INLINE p #-}
    p [] _ err = err
    p (x:xs) ok err = ok xs x

{-# INLINE char #-}
char :: Char -> Parser ()
char c = do
  c' <- next
  guard (c == c')

{-# INLINE eof #-}
eof :: Parser ()
eof = Peek (Parsing p) (\_ _ err -> err)
  where 
    {-# INLINE p #-}
    p [] ok err = ok [] ()
    p _ ok err = err

{-# INLINE skipMany #-}
skipMany :: Parser a -> Parser ()
skipMany p = p' where p' = (p >> p') `mplus` return ()

{-# INLINE run #-}
run :: Parser a -> String -> Maybe a
run p xs = unParsing (parsing p) xs (\_ -> Just) Nothing

runBrackets = run brackets
  where
    brackets :: Parser ()
    brackets = skipMany ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']')) *> eof