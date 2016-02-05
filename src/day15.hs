module Day15 where

import Control.Monad
import Data.List
import Data.Ord

-- capacity, durability, flavor, texture
type Ingredient = (Int, Int, Int, Int)

highest :: [Ingredient] -> Int
highest ingrds = maximum $ map score1 combinations
  where
    combinations = filter validQtts $ replicateM (length ingrds) [1..99]
      where validQtts = (== 100) . sum

    score1 qtts = product $ map ((`max` 0) . dotProduct) $ transpose ingList
      where
        dotProduct b = sum $ zipWith (*) qtts b
        ingList = map (\(a, b,c, d) -> [a, b, c, d]) ingrds
