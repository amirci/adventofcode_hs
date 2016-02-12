module Day2 where

import Data.List
import Data.List.Split

type Present = (Int, Int, Int)

paperWrap :: Present  -> Int
paperWrap (h, l, w) = doubleSides + minSide 
  where 
    doubleSides = sum $ map (*2) areas
    areas = [l*w, w*h, h*l]
    minSide = minimum areas

ribbon :: Present -> Int
ribbon (h, l, w) = wrapping + bow
  where
    wrapping = sum $ map (*2) smallestAreas
    smallestAreas = take 2 $ sort [h, l, w]
    bow = h * l * w


fromFile :: (Present -> Int) -> String -> IO Int
fromFile fn fileName = do
  contents <- readFile fileName
  return $ sum $ map (fn . readPresent) $ lines contents

  where
    separator = "x"
    toInts = read :: String -> Int
    readPresent = tuplify . map toInts . splitOn separator
    tuplify (a:b:c:[]) = (a, b, c)

