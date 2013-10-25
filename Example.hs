import Test
import Examples.Brackets
import Combinators

filename = "brackets"
example = brackets

main = test (readBS ("testdata/" ++ filename)) (example >> char '\n' >> eof)
