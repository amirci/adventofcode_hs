module Day15 where

import Control.Monad
import Data.List
import Data.Ord

-- capacity, durability, flavor, texture
type Ingredient = (Int, Int, Int, Int)

highest :: [Ingredient] -> Int
highest ingrds = maximum $ map score combinations
  where
    combinations = filter validQtts $ replicateM (length ingrds) [1..99]
    score = mulIgr . sumIgr . updateQtts
    validQtts = (== 100) . sum
    updateQtts = zipWith updateQt ingrds 
    updateQt (cap, dur, fl, tex) qt = [cap*qt, dur*qt, fl*qt, tex*qt]
    -- sumIgr = zipWith ((max 0) . (+))
    sumIgr = foldl1 $ zipWith (\a b -> max 0 (a+b)) 
    mulIgr = foldl1 (*)

