module Day17 where

import Data.Maybe
import Data.List

type Containers = [Int]

storeEggnog :: Int -> Containers -> Int
storeEggnog ltrs = length . filter ((== ltrs) . sum) . subsequences 
