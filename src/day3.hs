module Day3 where

import qualified Data.Text as T
import Data.List
import qualified Data.Set as Set


countPresents :: String -> Int
countPresents path = houses (0, 0) (Set.singleton (0, 0)) path
  where 
    houses last history []     = Set.size history
    houses last older   (x:xs) = cc (adjust last x) older xs
    cc next older xs = houses next (Set.insert next older) xs
    adjust (x, y) '^' = (x+1, y)
    adjust (x, y) 'v' = (x-1, y)
    adjust (x, y) '>' = (x, y+1)
    adjust (x, y) '<' = (x, y-1)
    adjust (x, y) _ = (x, y)


fromFile :: String -> IO Int
fromFile fileName = do
  contents <- readFile fileName
  return $ countPresents contents
  
