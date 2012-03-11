{-# LANGUAGE TypeFamilies #-}
module Base where

import Data.List

-- Token streams

class Stream a where
  type Token a
  primToken :: a -> (a -> Token a -> b) -> b -> (String -> b) -> b
  pos :: a -> Int

-- Reporting errors

expected :: [String] -> [String] -> [String]
expected unexpected [] = unexpected ++ ["Unknown error"]
expected unexpected expected =
  unexpected ++ [ "Expected " ++ list expected ]
  where list [exp] = exp
        list exp = intercalate ", " (init exp) ++ " or " ++ last exp
