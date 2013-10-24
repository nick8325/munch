{-# LANGUAGE NoMonomorphismRestriction #-}
module Brackets(brackets) where

import Control.Monad
import Control.Applicative
import Parsec
import Instances

brackets :: Parsec Chars ()
brackets = skipMany (checkpoint *> ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']')))

-- main = go brackets "testdata/brackets"
