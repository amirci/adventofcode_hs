module Day10 where

import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace



numCount :: String -> String
numCount [] = ""
numCount (a:xs) = parse (a, 1) xs
  where
    parse (n, count) [] = show count ++ [n]
    parse (n, count) (a:xs)
      | n == a = parse (n, count + 1) xs
      | otherwise = show count ++ [n] ++ numCount (a:xs)

