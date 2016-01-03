module Day6 where

import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Text as T
import qualified Data.Map as Map

type Light = (Int, Int)
type Brightness = Int

type LightBoard = Map.Map Light Brightness
type Range = [Light]

data Command = TurnOn | TurnOff | Toggle deriving (Show)

mkLight :: (Int, Int) -> Light
mkLight (a, b) = (a, b)

mkBoard :: LightBoard
mkBoard = Map.empty

turnOn :: Light -> LightBoard -> LightBoard
turnOn light board = Map.insert light 0 board

allOn :: LightBoard -> Range -> Bool
allOn board range = and $ map (flip isOn $ board) range

allOff :: LightBoard -> Range -> Bool
allOff board range = and $ map (flip isOff $ board) range

turnOff :: Light -> LightBoard -> LightBoard
turnOff = Map.delete

lightCount :: LightBoard -> Int
lightCount = Map.size

incBright :: Brightness -> Light -> LightBoard -> LightBoard
incBright amount light = Map.insertWith (+) light amount

decBright :: Int -> Light -> LightBoard -> LightBoard
decBright amount = Map.adjust decrease
  where decrease = (max 0) . (\a -> a - amount)

brightQ :: Light -> LightBoard -> Brightness
brightQ light = Map.findWithDefault 0 light

brightness :: LightBoard -> Brightness
brightness = Map.fold (+) 0

isOn :: Light -> LightBoard -> Bool
isOn = Map.member

isOff :: Light -> LightBoard -> Bool
isOff light board = not $ isOn light board

toggle :: Light -> LightBoard -> LightBoard
toggle light board = op light board
  where op = if (isOn light board) then turnOff else turnOn

lightIt :: String -> LightBoard -> LightBoard
lightIt instruction board = foldr (parseFn cmd) board coords 
  where
    (cmd, coords) = parse instruction
    parseFn TurnOn = turnOn
    parseFn TurnOff = turnOff
    parseFn Toggle = toggle

brightIt :: LightBoard -> String -> LightBoard
brightIt board instruction = foldr (parseFn cmd) board coords 
  where
    (cmd, coords) = parse instruction
    parseFn TurnOn  = incBright 1
    parseFn TurnOff = decBright 1
    parseFn Toggle  = incBright 2

parse :: String -> (Command, [Light])
parse instruction = (cmd, coords)
  where
    coords = [(i, j) | i <- [x..z], j <- [y..w]]  
    (cmd, (x:y:z:w:[])) = mapCoord $ parse' words
    mapCoord (cmd, begin, end) = (cmd, coord begin ++ coord end)
    parse' ("turn":"on":start:_:end:[])  = (TurnOn, start, end)
    parse' ("turn":"off":start:_:end:[]) = (TurnOff, start, end)
    parse' ("toggle":start:_:end:[])     = (Toggle, start, end)
    words = map T.unpack $ T.splitOn (T.pack " ") $ T.pack instruction
    readInt = read::String->Int
    comma = T.pack ","
    coord str = map (readInt . T.unpack) $ T.splitOn comma $ T.pack str

