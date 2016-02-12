module Day21 where

import Data.List
import Data.Ord
import Data.Maybe
import Debug.Trace

data Player = Player { hp::Int, damp::Int, armorp::Int }

type Item = [Int]

mkQ :: Int -> Int -> Int -> Item
mkQ a b c = [a, b, c]

rings :: [Item]
rings = [mkQ 25 1 0, mkQ 50 2 0, mkQ 100 3 0, mkQ 20 0 1, mkQ 40 0 2 , mkQ 80 0 3]

weapons :: [Item]
weapons = [mkQ 8 4 0, mkQ 10 5 0, mkQ 25 6 0, mkQ 40 7 0, mkQ 74 8 0]

armor :: [Item]
armor = [mkQ 13 0 1, mkQ 31 0 2, mkQ 53 0 3, mkQ 75 0 4, mkQ 102 0 5]

armory :: [[Item]]
armory = [ [w, a, r1, r2] | 
  w <- weapons, a <- zero:armor, r1 <- zero:rings, r2 <- zero:rings,
  (r1 == zero && r2 == zero || r1 /= r2) ]
  where zero = [0,0,0]

minGold :: Player -> Player -> Int
minGold p1 p2 = fst $ minimumBy (comparing fst) $ filter p1Won rounds
  where
    battle' = battle p1 p2
    p1Won (_, (hp1, hp2)) = hp2 <= 0
    rounds = map battle' armory

maxGold :: Player -> Player -> Int
maxGold p1 p2 = fst $ maximumBy (comparing fst) $ filter p2Won rounds
  where
    battle' = battle p1 p2
    p2Won (_, (hp1, hp2)) = hp1 <= 0
    rounds = map battle' armory

battle :: Player -> Player -> [Item] -> (Int, (Int, Int))
battle Player{hp=hp1}
       Player{hp=hp2, damp=damP2, armorp=armrP2}
       items
       = (cost, (finalHp1, finalHp2))
  where
    [cost, damg, armr] = map sum $ transpose items
    (finalHp1, finalHp2, _) = fromJust $ find aWinner $ iterate battle' (hp1, hp2, True)
    [p1Dmg, p2Dmg] = map (max 1) [damP2 - armr, damg - armrP2]
    aWinner (a, b, _) = a <= 0 || b <= 0
    battle' (hp1, hp2, turn)
      | not turn = (hp1 - p1Dmg, hp2, not turn)
      | turn = (hp1, hp2 - p2Dmg, not turn)
