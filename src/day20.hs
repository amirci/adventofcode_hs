module Day20 where

import Data.List

lowestHouse :: Int -> Int
lowestHouse total = fst $ head $ filter ((>= total) . snd) houses
  where houses = zip [1..] presents

presents :: [Int]
presents = map ((* 10) . sum . divisors) [1..]
  
divisors :: Int -> [Int]  
-- too slow when using n / 2 
--divisors 1 = [1]
--divisors n = (1 : filter ((==0) . rem n) [2 .. n `div` 2]) ++ [n]
divisors h = nub $ div' ++ map (div h) div'
  where div' = filter ((== 0) . rem h) [1..isqrt h]

isqrt :: Int -> Int
isqrt = ceiling . sqrt . fromIntegral
