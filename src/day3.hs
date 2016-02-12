module Day3 where

import Data.List
import qualified Data.Set as Set

type Address = (Int, Int)

santaHouses :: String -> Int
santaHouses = Set.size . visitHouses

visitHouses :: String -> Set.Set Address
visitHouses path = Set.fromList $ foldl addresses [(0, 0)] deltas
  where 
    addresses (last:rest) delta = (last `nextHouse` delta):last:rest
    deltas = map dir path
    nextHouse (a, b) (c, d) = (a+c, b+d)
    dir '^' = (1 , 0)
    dir 'v' = (-1, 0)
    dir '>' = (0 , 1)
    dir '<' = (0 , -1)
    dir  _  = (0 , 0)

robotHouses :: String -> Int
robotHouses path = Set.size $ foldl1 Set.union $ map visitHouses $ splitPaths path ([], [])
  where
    splitPaths [] (p1, p2) = [p1, p2]
    splitPaths (x:[]) (p1, p2) = [p1 ++ [x], p2]
    splitPaths (x:y:xs) (p1, p2) = splitPaths xs (p1 ++ [x], p2 ++ [y])
