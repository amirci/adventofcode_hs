module Day7 where

import Text.Read
import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.Word
import qualified Data.Map as Map

type Value = Word16
type Circuit = String
type BoardCmd = String
type CircuitMap = Map.Map Circuit Value
type PendingCmds = [BoardCmd]
type CircuitBoard = (CircuitMap, PendingCmds)

mkBoard :: CircuitBoard
mkBoard = (Map.empty, [])

wire :: Circuit -> CircuitBoard -> Maybe Value
wire w (circuits, _) = Map.lookup w $ circuits 

addCircuit :: BoardCmd -> CircuitBoard -> CircuitBoard
addCircuit cmd (c, p) = reEval c [] (cmd:p)
  where
    reEval circ failing [] = (circ, failing)
    reEval circ failing (cmd:xs) = 
      case apply cmd circ of
        Just circ -> reEval circ [] (failing ++ xs)
        Nothing   -> reEval circ (failing ++ [cmd]) xs


apply :: BoardCmd -> CircuitMap -> Maybe CircuitMap
apply cmd circ = update <$> parse tokens
  where
    tokens = splitOn " " cmd
    targetWire = last tokens
    wire x = Map.lookup x circ
    maxVal = (maxBound :: Word16) + 1

    update val = Map.insert targetWire val circ
      
    parse (x:"->":c:[])            = wireOrValue x
    parse ("1":"AND":x:"->":c:[])  = (.&. 1) <$> wire x
    parse (x:"AND":y:"->":c:[])    = (.&.)  <$> wire x <*> wire y 
    parse (x:"OR":y:"->":c:[])     = (.|.)  <$> wire x <*> wire y 
    parse (x:"LSHIFT":v:"->":c:[]) = shiftL <$> wire x <*> (readMaybe v :: Maybe Int)
    parse (x:"RSHIFT":v:"->":c:[]) = shiftR <$> wire x <*> (readMaybe v :: Maybe Int)
    parse ("NOT":x:"->":c:[])      = ((+) maxVal . complement) <$> wire x 
    parse _ = Nothing

    wireOrValue val = case (readMaybe val :: Maybe Value) of
      Just num -> Just num
      Nothing  -> wire val

-- parse :: BoardCmd -> Command
-- parse cmd = parse' tokens
--   where
--     tokens = splitOn " " cmd
--     parse' (1:"AND":x:"->":c:[])    = AssignCmd x c
--     parse' (x:"AND":y:"->":c:[])    = BinCmd (.&.) x y c
--     parse' (x:"OR":y:"->":c:[])     = BinCmd (.|.) x y c
--     parse' (x:"LSHIFT":v:"->":c:[]) = ShiftCmd shiftL x (read v :: Int) c
--     parse' (x:"RSHIFT":v:"->":c:[]) = ShiftCmd shiftR x (read v :: Int) c
--     parse' ("NOT":x:"->":c:[])      = UnCmd complement x c
--     parse' (x:"->":c:[])            = 
--       case (readMaybe x :: Maybe Int) of
--         Just num -> InitCmd num c 
--         Nothing  -> AssignCmd x c

