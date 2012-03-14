module Main where

import Prelude hiding (takeWhile)
import Data.Char
import qualified Data.ByteString.Char8 as BS
import Run
import Control.Monad(liftM2)

data Expr = Var BS.ByteString | Fun BS.ByteString [Expr]

parser :: Parser Expr
parser = checkpoint *> spaced (fmap Var var <|> liftM2 Fun fun args <|> parens parser)

satisfy_ p = satisfy p >> return ()

{-# INLINE spaced #-}
spaced p = aux where aux = (satisfy (== ' ') >> aux) <|> p
{-# INLINE parens #-}
parens p = satisfy (== '(') *> p <* satisfy (== ')') <?> "bracketed expression"

var = takeWhile1 isUpper' <?> "variable"
fun = takeWhile1 isLower' <?> "function"

isUpper' c = c >= 'A' && c <= 'Z'
isLower' c = c >= 'a' && c <= 'z'

args = spaced (parens (sepBy1 parser (spaced comma)) <|> return [])
comma = satisfy (== ',') <?> "comma"

main = go (skipMany parser) "testdata/prolog"
