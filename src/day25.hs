module Day25 where

type Code = Int

nth :: Int -> Int -> Int
nth row col = row' * (row' - 1) `div` 2 + col
  where row' = row + col - 1

codes :: Int -> [Code]
codes = iterate newCode
  where newCode oldCode = oldCode * 252533 `mod` 33554393
