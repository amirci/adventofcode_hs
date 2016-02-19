module Day7 where

import Text.Read
import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Word
import qualified Data.Map as Map

type Value = Word16
type Circuit = String
type BoardCmd = String
type CircuitBoard = Map.Map Circuit Value

mkBoard :: CircuitBoard
mkBoard = Map.empty

wire :: Circuit -> CircuitBoard -> Maybe Value
wire = Map.lookup

run :: [BoardCmd] -> CircuitBoard -> CircuitBoard
run cmds board = untilNoCmdIsPending $ iterate run' (board, pending)
  where
    untilNoCmdIsPending = fst . fromJust . find (null . snd)
    pending = map parse cmds
    run' (cb, pending) = foldl run'' (cb, []) pending
    run'' (cb, failed) cmd = 
      case cmd cb of
        Just board -> (board, failed)
        Nothing    -> (cb, cmd:failed)

parse :: BoardCmd -> (CircuitBoard -> Maybe CircuitBoard)
parse cmd = assign target $ parse' tokens
  where
    tokens = splitOn " " cmd
    target = last tokens

    assign target fn board = update <$> fn board
      where update val = Map.insert target val board

    expr val board = case (readMaybe val :: Maybe Value) of
      Just num -> Just num
      Nothing  -> wire val board

    binFn fn x y board = fn <$> expr x board <*> expr y board

    parse' [x, "->"    , c]          = expr x
    parse' [x, "AND"   , y, "->", c] = binFn (.&.) x y
    parse' [x, "OR"    , y, "->", c] = binFn (.|.) x y
    parse' [x, "LSHIFT", v, "->", c] = \board -> shiftL <$> expr x board <*> (readMaybe v :: Maybe Int)
    parse' [x, "RSHIFT", v, "->", c] = \board -> shiftR <$> expr x board <*> (readMaybe v :: Maybe Int)
    parse' ["NOT", x, "->", c]       = \board -> ((+) maxVal . complement) <$> wire x board 
      where maxVal = (maxBound :: Word16) + 1
    parse' _ = \board -> Nothing


