module Day20 where

import Data.List

lowest :: [Int] -> Int -> Int
lowest presents total = fst $ head $ filter ((>= total) . snd) houses
  where houses = zip [1..] presents

presents :: Int -> (Int->[Int]) -> [Int]
presents amount divisors = map ((* amount) . sum . divisors) [1..]

presentsA :: [Int]
presentsA = presents 10 divisors
  
presentsB :: [Int]
presentsB = presents 11 divisors50

divisors :: Int -> [Int]  
divisors h = nub $ div' ++ map (div h) div'
  -- too slow when using n / 2 switched to sqrt
  where div' = filter ((== 0) . rem h) [1..isqrt h]

divisors50 :: Int -> [Int]  
divisors50 h = dropWhile (\e -> h >= 50 * e) $ sort $ divisors h

isqrt :: Int -> Int
isqrt = ceiling . sqrt . fromIntegral
