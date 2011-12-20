{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, BangPatterns #-}
module Main(main) where

import Control.Applicative
import Control.Monad
import Parsec
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSW
import BS

{-# INLINE peek #-}
peek :: Stream a b => Parsec a b
peek = Parsec (\ok err inp exp -> primToken inp (\_ x -> ok x err inp exp) (err exp) (Error (position inp)))

{-# INLINE prim #-}
prim :: (forall c. a -> (b -> a -> c) -> c -> c) -> Parsec a b
prim p =
  Parsec (\ok err inp exp ->
    p inp (\x inp' -> ok x err inp' exp) (err exp))

{-# INLINE prim' #-}
prim' :: (a -> Maybe (b, a)) -> Parsec a b
prim' p =
  prim (\x ok err -> case p x of { Nothing -> err; Just (y, z) -> ok y z })

{-# INLINE satisfy' #-}
satisfy' p = do
  x <- next
  guard (p x)
  return x
-- satisfy' = satisfy

{-# INLINE char #-}
char :: Char -> Parsec BS.ByteString Char
char x = satisfy' (== x)
{-# INLINE isDigit #-}
isDigit x = x >= '0' && x <= '9'
-- isDigit '0' = True
-- isDigit '1' = True
-- isDigit '2' = True
-- isDigit '3' = True
-- isDigit '4' = True
-- isDigit '5' = True
-- isDigit '6' = True
-- isDigit '7' = True
-- isDigit '8' = True
-- isDigit '9' = True
-- isDigit _ = False
{-# INLINE digit #-}
digit = satisfy' isDigit
many1 = some
parse p x = run (const []) p

expr = chainl1 term ((+) <$ char '+' <|> (-) <$ char '-')

term = chainl1 fact ((*) <$ char '*' <|> div <$ char '/')

fact =  cut' $ char '(' *> expr <* char ')' <|> number
-- number = do
--   -- c <- peek
--   -- guard (isDigit c)
--   -- cut
--   prim' BS.readInt

{-# INLINE number #-}
number = do
  c <- peek
  guard (isDigit c)
  --cut
  prim (\inp ok err -> loop ok 0 inp)

-- number = do
--   c <- peek
--   guard (isDigit c)
--   cut
--   bs <- prim (\inp ok err -> uncurry ok (BS.span isDigit inp))
--   prim (\inp ok err -> ok (loop' 0 bs) inp)

loop :: forall c. (Int -> BS.ByteString -> c) -> Int -> BS.ByteString -> c
loop k = aux
  where aux !n ps =
          case BS.uncons ps of
            Nothing -> k n ps
            Just (c, ps') | isDigit c ->
              aux (n*10 + (fromEnum c - fromEnum '0')) ps'
            Just _ -> k n ps

-- loop' :: Int -> BS.ByteString -> Int
-- loop' !n ps =
--   case BS.uncons ps of
--     Nothing -> n
--     Just (c, ps') -> loop' (n*10 + (fromEnum c - fromEnum '0')) ps'

eval :: BS.ByteString -> Int
eval = either (error . show) id . snd . parse expr ""

{-# INLINE chainl1 #-}
chainl1 :: Stream a b => Parsec a c -> Parsec a (c -> c -> c) -> Parsec a c
chainl1 p op = cut' p >>= rest where
  rest x = do f <- cut' op
              y <- cut' p
              rest $! (f x y)
           <|> pure x

main :: IO ()
main = do
    file <- BS.readFile "expr"
    putStr $ show $ eval file
    putStr "\n"
