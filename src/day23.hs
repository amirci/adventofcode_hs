module Day23 where

import Data.List
import Data.List.Split
import Data.Maybe

data Computer = Computer { rega, regb :: Int } deriving (Show, Eq)
type State = (Computer, Int)
type Instruction = State -> State
type Cmd = String

bootUp :: Computer
bootUp = Computer 0 0

run :: [Cmd] -> Computer -> Computer
run input cpt = fst $ fromJust $ find ((== len) . snd) $ iterate run'' (cpt, 0)
  where
    len = length input
    cmds = map parse input
    run'' inst@(cpt, i)
      | i >= len = (cpt, len)
      | otherwise = (cmds !! i) inst

parse :: Cmd -> Instruction
parse input = parse' tokens
  where 
    tokens = (splitOn " " . delete ',' . delete '+') input

    jmpIf cpt "a" pred val = if pred $ rega cpt then (read val) else 1
    jmpIf cpt "b" pred val = if pred $ regb cpt then (read val) else 1

    rep cpt "a" fn = cpt {rega=fn $ rega cpt}
    rep cpt "b" fn = cpt {regb=fn $ regb cpt}

    isEven = (== 0) . (flip mod 2)

    parse' ["inc", r] = \(c, i) -> (rep c r $ (+ 1), i+1)
    parse' ["hlf", r] = \(c, i) -> (rep c r $ flip div 2, i+1)
    parse' ["tpl", r] = \(c, i) -> (rep c r $ (* 3), i+1)
    parse' ["jmp", o] = \(c, i) -> (c, i + read o)
    parse' ["jio", r, o] = \(c, i) -> (c, i + jmpIf c r (== 1) o)
    parse' ["jie", r, o] = \(c, i) -> (c, i + jmpIf c r isEven o)
