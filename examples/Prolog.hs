module Prolog where

-- import Prelude hiding (takeWhile)
import Data.Char
-- import qualified Data.ByteString.Char8 as BS
-- import Run
import Control.Monad
import Control.Applicative
import Parsec
import Instances

data Expr = Var Char | Fun Char [Expr]

parser :: Parsec Chars Expr
parser = checkpoint *> spaced (fmap Var var <|> liftM2 Fun fun args <|> parens parser)

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

-- main = go (skipMany parser) "testdata/prolog"
