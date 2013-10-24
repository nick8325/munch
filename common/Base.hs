{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Base where

import Data.List

-- Token streams

class Stream a where
  type Token a
  primToken :: a -> (a -> Token a -> b) -> b -> (String -> b) -> b
  hd :: a -> Token a
  append :: a -> a -> a
  pos :: a -> Int

  data Fun a :: * -> *
  abs :: (a -> b) -> Fun a b
  app :: Fun a b -> a -> b

-- Reporting errors

expected :: [String] -> [String] -> [String]
expected unexpected [] = unexpected ++ ["Unknown error"]
expected unexpected expected =
  unexpected ++ [ "Expected " ++ list expected ]
  where list [exp] = exp
        list exp = intercalate ", " (init exp) ++ " or " ++ last exp
