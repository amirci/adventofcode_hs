module Day15 where

import Control.Monad
import Data.List
import Data.Ord

-- capacity, durability, flavor, texture, calories
type Ingredient = (Int, Int, Int, Int, Int)
type Quantities = [Int]
type ScoreFn = [Ingredient] -> Quantities -> Int


highest' :: ScoreFn -> [Ingredient] -> Int
highest' scoreFn ingrds = maximum $ map (scoreFn ingrds) combinations
  where
    combinations = filter validQtts $ replicateM (length ingrds) [1..99]
    validQtts = (== 100) . sum

score :: ScoreFn
score ingrds qtts = product $ map ((max 0) . dotProduct) $ transpose ingList
  where
    dotProduct b = sum $ zipWith (*) qtts b
    ingList = map (\(a, b,c, d, _) -> [a, b, c, d]) ingrds


scoreCal :: ScoreFn
scoreCal ingrds qtts = calCheck $ map ((max 0) . dotProduct) $ transpose ingList
  where
    calCheck xs
      | last xs == 500 = product $ init xs
      | otherwise = 0
    dotProduct b = sum $ zipWith (*) qtts b
    ingList = map (\(a, b,c, d, e) -> [a, b, c, d, e]) ingrds

