module Day9 where

import Data.List
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.MultiMap as MMap
import Data.Maybe
import Debug.Trace

type City = String

type Path = (City, City, Int)
type Destination = (City, Int)
type Graph = MMap.MultiMap City Destination

mkPath :: String -> Path
mkPath str = parse $ splitOn " " str
  where parse (c1:"to":c2:"=":dist:[]) = (c1, c2, read dist :: Int)

shortest :: [Path] -> Int
shortest paths = minimum $ distances paths

longest :: [Path] -> Int
longest paths = maximum $ distances paths

distances :: [Path] -> [Int]
distances paths = map fromJust $ actual
  where
    sumDists path = fmap sum $ sequence path
    reachable (a:b:[]) = [Map.lookup (a, b) dists]
    reachable (a:b:xs) = (Map.lookup (a, b) dists) : (reachable (b:xs))
    actual = filter isJust $ map (sumDists . reachable) allPaths 
    allPaths = permutations $ nub $ fst $ unzip $ Map.keys dists
    inst (c1, c2, dist) = Map.insert (c1, c2) dist . Map.insert (c2, c1) dist
    dists = foldr inst Map.empty paths

