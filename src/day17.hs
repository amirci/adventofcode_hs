module Day17 where

import Data.Maybe
import Data.List

type Containers = [Int]

storeEggnog :: Int -> Containers -> Int
storeEggnog ltrs ctnrs = length $ fitting ltrs ctnrs

fitting :: Int -> Containers -> [Containers]
fitting ltrs = filter ((== ltrs) . sum) . subsequences 

storeMin :: Int -> Containers -> Int
storeMin ltrs ctnrs = length $ filter ((==) min . length) $ fitting ltrs ctnrs
  where
    min = minimum $ map length $ fitting ltrs ctnrs
