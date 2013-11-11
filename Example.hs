import Test
import Examples.Brackets
import qualified Examples.Prolog
import Combinators

filename = "brackets"
example = brackets

main = test (readBS ("testdata/" ++ filename)) (example >> char '\n' >> eof)
