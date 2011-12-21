module Test where

test :: Show a => (String -> IO b) -> (c -> b -> a) -> c -> String -> IO ()
test readFile parse p name = do
  file <- readFile name
  print (parse p file)