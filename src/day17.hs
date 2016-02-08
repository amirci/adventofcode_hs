module Day17 where

import Data.Maybe
import Data.List

type Containers = [Int]

storeEggnog :: Int -> Containers -> Int
storeEggnog ltrs = length . fitting ltrs

fitting :: Int -> Containers -> [Containers]
fitting ltrs = filter ((== ltrs) . sum) . subsequences 

storeMin :: Int -> Containers -> Int
storeMin ltrs ctnrs = length $ filter ((==) min . length) fit
  where
    fit = fitting ltrs ctnrs 
    min = minimum $ map length fit
