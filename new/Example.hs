import Test
import Brackets
import Combinators

filename = "brackets"
example = brackets

main = test (readBS ("../testdata/" ++ filename)) (example >> char '\n' >> eof)