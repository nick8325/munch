module Examples.Brackets where

import Control.Applicative
import Class
import Combinators
import Simple
import Look
import qualified Data.ByteString.Char8 as B

brackets :: Look (Simple B.ByteString) ()
brackets = skipMany ((char '(' *> brackets <* char ')') <|> (char '[' *> brackets <* char ']'))
