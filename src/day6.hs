module Day6 where

import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Text as T
import qualified Data.Set as Set

type Light = (Int, Int)
type LightBoard = Set.Set Light


turnOn :: Light -> LightBoard -> LightBoard
turnOn = Set.insert

turnOff :: Light -> LightBoard -> LightBoard
turnOff = Set.delete

lightCount :: LightBoard -> Int
lightCount = Set.size

isOn :: Light -> LightBoard -> Bool
isOn = Set.member

isOff :: Light -> LightBoard -> Bool
isOff light board = not $ isOn light board

toggle :: Light -> LightBoard -> LightBoard
toggle light board = op light board
  where op = if (isOn light board) then turnOff else turnOn

command :: String -> LightBoard -> LightBoard
command cmd board = foldr fn board coords 
  where
    coords = [(i, j) | i <- [x..z], j <- [y..w]]  
    (fn, (x:y:z:w:[])) = mapCoord $ parse words
    mapCoord (fn, begin, end) = (fn, coord begin ++ coord end)
    parse ("turn":"on":start:_:end:[])  = (turnOn, start, end)
    parse ("turn":"off":start:_:end:[]) = (turnOff, start, end)
    parse ("toggle":start:_:end:[])     = (toggle, start, end)
    words = map T.unpack $ T.splitOn (T.pack " ") $ T.pack cmd
    readInt = read::String->Int
    comma = T.pack ","
    coord str = map (readInt . T.unpack) $ T.splitOn comma $ T.pack str

