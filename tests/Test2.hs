module Test2(parser) where

import Data.Char
import Test1
import Parsec
import Control.Applicative
import Control.Monad

parser :: Parsec Chars Expr
parser = spaced (fmap Var var <|> liftM2 Fun fun args <|> parens parser)

satisfy_ p = satisfy p >> return ()

{-# INLINE spaced #-}
spaced p = aux where aux = (satisfy (== ' ') >> aux) <|> p
{-# INLINE parens #-}
parens p = between (satisfy (== '(')) (satisfy (== ')')) p <?> "bracketed expression"

-- var = satisfy isUpper' <?> "variable"
-- fun = satisfy isLower' <?> "function"
var = satisfy (== 'X') <?> "variable"
fun = satisfy (== 'f') <?> "function"

isUpper' c = c >= 'A' && c <= 'Z'
isLower' c = c >= 'a' && c <= 'z'

args = spaced (parens (sepBy1 parser (spaced comma)) <|> return [])
comma = satisfy (== ',') <?> "comma"
-- comma = return ()
