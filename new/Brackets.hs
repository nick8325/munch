module Brackets where

import Control.Applicative
import Class
import Combinators
import Simple
import Look3
import qualified Data.ByteString.Char8 as B

{-# INLINEABLE brackets #-}
brackets :: Look (Simple B.ByteString) ()
brackets = skipMany ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']'))