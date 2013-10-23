{-# LANGUAGE RankNTypes #-}
module Test(brackets) where

import Control.Applicative
import Control.Monad

newtype Parser a = Parser { unParser :: forall b c p. Parsing b c p a }

type List b a = (a -> b -> b) -> b -> b

data Parsing b c p a = Parsing {
  stop :: (a -> c) -> c -> c,
  feed :: Char -> (forall a. Parsing b c p a -> (a -> p b) -> p b) -> 
                  (a -> p b) -> List c (p b)
  }

instance Monad (Parsing b c p) where
  {-# INLINE return #-}
  return = parserReturn
  {-# INLINE (>>=) #-}
  (>>=) = parserBind

{-# INLINE parserReturn #-}
parserReturn x = Parsing (\ok _ -> ok x) (\_ _ _ _ e -> e)

{-# INLINE parserBind #-}
p `parserBind` f =
  Parsing (\ok err -> stop p (\x -> stop (f x) ok err) err)
          (\ch b r op e -> stop p (\x -> feed (f x) ch b r op e)
                                  (feed p ch b (\x -> b (f x) r) op e))

instance MonadPlus (Parsing b c p) where
  {-# INLINE mzero #-}
  mzero = parserZero
  {-# INLINE mplus #-}
  mplus = parserPlus

{-# INLINE parserZero #-}
parserZero = Parsing (\_ e -> e) (\_ _ _ _ e -> e)

{-# INLINE parserPlus #-}
p1 `parserPlus` p2 =
  Parsing (\ok err -> stop p1 ok (stop p2 ok err))
  (\c b r op e -> feed p1 c b r op (feed p2 c b r op e))

instance Monad Parser where
  return x = Parser (return x)
  Parser x >>= f = Parser (x >>= unParser . f)

instance MonadPlus Parser where
  mzero = Parser mzero
  Parser p `mplus` Parser q = Parser (p `mplus` q)

instance Functor Parser where
  {-# INLINE fmap #-}
  fmap f m = do { x <- m; return (f x) }

instance Applicative Parser where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  mf <*> mx = do { f <- mf; x <- mx; return (f x) }

instance Alternative Parser where
  {-# INLINE empty #-}
  empty = mzero
  {-# INLINE (<|>) #-}
  (<|>) = mplus

{-# INLINE next #-}
next :: Parser Char
next = Parser (Parsing (\_ err -> err) (\c b r op e -> r c `op` e))

{-# INLINE char #-}
char :: Char -> Parser ()
char c = do
  c' <- next
  guard (c == c')

{-# INLINE eof #-}
eof :: Parser ()
eof = Parser (Parsing (\ok _ -> ok()) (\_ _ _ _ e -> e))

{-# INLINE skipMany #-}
skipMany :: Parser a -> Parser ()
skipMany p = p' where p' = (p >> p') `mplus` return ()
                      
brackets :: Parser ()
brackets = skipMany ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']')) *> eof