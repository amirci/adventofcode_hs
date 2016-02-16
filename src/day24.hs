module Day24 where

import Data.List
import Control.Monad

type Quantum = Int
type Quantity = Int
type Section = (Quantity, Quantum)
type Partition = [[Present]]
type Present = Int

groups :: [Int] -> Section
groups presents = head $ sort $ map section $ partitions presents
  where
    section xs = (ml, quantum)
      where 
        quantum = product $ map sum xs
        ml = minimum $ map length xs
    
partitions :: [Present] -> [Partition]    
partitions presents = filter isValid allp
  where
    isValid = valid presents
    allp = [[i, j, k] | i<-pset, j<-pset, k<-pset]
    pset = subsequences presents

valid :: [Present] -> Partition -> Bool
valid presents pt = allSame pt && sameSets pt
  where
    allSame xs = sameSum $ map sum xs
    sameSum xs = and $ map (== head xs) (tail xs)
    sameSets xs = same' presents $ concat xs
    same' a b = sort a == sort b

