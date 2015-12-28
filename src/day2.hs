module Day2 where

import qualified Data.Text as T
import Data.List

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


fromFile :: (Present->Int) -> String -> IO Int
fromFile fn fileName = do
  contents <- readFile fileName
  return $ sum $ map (fn . readPresent) $ lines contents

  where
    separator = T.pack "x"
    toInts = (read::String->Int) . T.unpack
    readPresent = tuplify . map toInts . T.splitOn separator . T.pack
    tuplify (a:b:c:[]) = (a, b, c)
    tuplify _ = (0, 0, 0)

