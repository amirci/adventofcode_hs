module Day24 where

import Data.List
import Control.Monad
import Data.Ord

type Quantum = Int
type Quantity = Int
type Section = (Quantity, Quantum)
type Partition = [[Present]]
type Present = Int

minQ :: [Int] -> Int -> Quantum
minQ presents pts = minimum $ map product $ filter ((== min) . length) $ partitions
  where
    weight = sum presents
    secWeight = weight `div` pts
    partitions = filter ((==) secWeight . sum) $ subsequences presents
    min = length $ minimumBy (comparing length) partitions

-- finds num partitions matching weight (not necessary)
findPt :: [Present] -> Int -> Int -> [Partition]
findPt presents weight num 
  | num == 1 = [partitions]
  | otherwise = concatMap findRest partitions
  where
    sameSet a b = (sort . concat) a == (sort . concat) b
    partitions = filter ((==) weight . sum) $ subsequences presents
    findRest part = nubBy sameSet $ zipWith (:) (repeat part) found
      where 
        diff = presents \\ part
        found = findPt diff weight (num-1)
    
