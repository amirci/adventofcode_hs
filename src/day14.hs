module Day14 where

import Data.Ord
import Data.List
import Debug.Trace

type Reindeer = (Int, Int, Int)

mkRd :: Int -> Int -> Int -> Reindeer
mkRd speed fly rest = (speed, fly, rest)

longest :: [Reindeer] -> Int -> Int
longest rdrs kms = maximum $ map dist rdrs
  where dist = flip distance kms

distance :: Reindeer -> Int -> Int
distance rd@(speed, fly, resting) kms = times * fly * speed + lastFly * speed
  where
    (times, rest) = divMod kms (fly + resting)
    lastFly = min rest fly

winner :: [Reindeer] -> Int -> (Reindeer, Int)
winner rdrs kms = maximumBy (comparing snd) $ points rdrs kms

points :: [Reindeer] -> Int -> [(Reindeer, Int)]
points rdrs kms = winner' init kms
  where
    init = map (\r -> (r, 0)) rdrs
    winner' rs 0 = rs
    winner' rs kms = winner' (traceShow updated updated) (kms - 1)
      where 
        distFn = (flip distance kms) . fst
        incWinner ((rd, pts):xs) = (rd, pts + 1):xs
        updated = incWinner $ reverse $ sortBy (comparing distFn) rs


