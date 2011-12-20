{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}
module Main(main) where

import Control.Applicative
import Parsec
import qualified Data.ByteString.Char8 as BS
import BS
import Data.Maybe
import Control.Monad

{-# INLINE prim #-}
prim :: (forall c. a -> (b -> a -> c) -> c -> c) -> Parsec a b
prim p =
  Parsec (\ok err inp exp ->
    p inp (\x inp' -> ok x err inp' exp) (err exp))

{-# INLINE prim' #-}
prim' :: (a -> Maybe (b, a)) -> Parsec a b
prim' p =
  prim (\x ok err -> case p x of { Nothing -> err; Just (y, z) -> ok y z })

{-# INLINE char #-}
char :: Char -> Parsec BS.ByteString Char
char x = satisfy (== x)
{-# INLINE isDigit #-}
isDigit x = x >= '0' && x <= '9'
{-# INLINE digit #-}
digit = satisfy isDigit
many1 = some
parse p x = run (const []) p

expr = chainl1 term ((+) <$ char '+' <|> (-) <$ char '-')

term = chainl1 fact ((*) <$ char '*' <|> div <$ char '/')


{-# INLINE peek #-}
peek :: Stream a b => Parsec a b
peek = Parsec (\ok err inp exp -> primToken inp (\_ x -> ok x err inp exp) (err exp) (Error (position inp)))

fact = number <|> char '(' *> expr <* char ')'
  where {-# INLINE number #-}
        number = do
          x <- prim (\inp ok err -> uncurry ok (BS.span isDigit inp))
          guard (not (BS.null x))
          cut
          return (fst (fromJust (BS.readInt x)))

eval :: BS.ByteString -> Int
eval = either (error . show) id . snd . parse expr ""

{-# INLINE chainl1 #-}
chainl1 :: (Monad f, Alternative f) => f a -> f (a -> a -> a) -> f a
chainl1 p op = p >>= rest where
  rest x = do f <- op
              y <- p
              rest $! (f x y)
           <|> pure x

main :: IO ()
main = do
    file <- BS.readFile "expr"
    putStr $ show $ eval file
    putStr "\n"
