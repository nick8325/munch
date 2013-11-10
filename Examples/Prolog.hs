module Examples.Prolog where

import Control.Applicative
import Control.Monad
import Class
import Combinators
import Simple(Simple)
import Look
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Char

data Expr = Var Char | Fun Char [Expr]

parser :: Look (Simple B.ByteString) Expr
parser = spaced (fmap Var var <|> liftM2 Fun fun args <|> parens parser)

{-# INLINE satisfy_ #-}
satisfy_ p = satisfy p >> return ()

{-# INLINE spaced #-}
spaced p = aux where aux = (satisfy (== ' ') >> aux) <|> p
{-# INLINE parens #-}
parens p = satisfy (== '(') *> p <* satisfy (== ')') <?> "bracketed expression"

{-# INLINE var #-}
var = satisfy isUpper' <?> "variable"
{-# INLINE fun #-}
fun = satisfy isLower' <?> "function"

{-# INLINE isUpper' #-}
isUpper' c = c >= 'A' && c <= 'Z'
{-# INLINE isLower' #-}
isLower' c = c >= 'a' && c <= 'z'

{-# INLINE args #-}
args = spaced (parens (sepBy1 parser (spaced comma)) <|> return [])
{-# INLINE comma #-}
comma = satisfy (== ',') <?> "comma"