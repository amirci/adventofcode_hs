module Day18 where

import Data.Maybe
import Data.List
import Control.Monad

type Light = (Int, Int)
type Board = (Int, Int, [Light])

lightCount :: Board -> Int
lightCount board = length $ cells board

cells :: Board -> [Light]
cells (_, _, c) = c

mkBoard :: [String] -> Board
mkBoard lines = (rows, cols, cells)
  where
    cells = concat $ zipWith mkRow lines [1..]
    rows = length lines
    cols = length $ head lines
    mkRow line row = catMaybes $ zipWith mkCol line [1..]
      where 
        mkCol '#' col = Just (row, col)
        mkCol _ _     = Nothing


neighbours :: Light -> [Light]
neighbours (a, b) = [(a+i,b+j) | i<-[-1..1], j<-[-1..1], (i, j) /= (0, 0)]

evolve :: Board -> Board
evolve (rows, cols, cells) = (rows, cols, newCells)
  where
    newCells = map fst $ filter alive frequencies
    alive (cell, n) = (n == 3) || (n == 2 && cell `elem` cells)
    frequencies = filter inRange $ freq $ concatMap neighbours cells 
      where 
        freq cells = [(head x, length x) | x <- group $ sort cells]
        inRange ((r, c), _) = r > 0 && r <= rows && c > 0 && c <= cols

