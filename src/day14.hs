module Day14 where

import Data.Ord
import Data.List

type Reindeer = (Int, Int, Int)

mkRd :: Int -> Int -> Int -> Reindeer
mkRd speed fly rest = (speed, fly, rest)

longest :: [Reindeer] -> Int -> Int
longest rdrs seconds = maximum $ map dist rdrs
  where dist = flip distance seconds

distance :: Reindeer -> Int -> Int
distance rd@(speed, fly, resting) seconds = times * fly * speed + lastFly * speed
  where
    (times, rest) = divMod seconds (fly + resting)
    lastFly = min rest fly

winner :: [Reindeer] -> Int -> (Reindeer, Int)
winner rdrs seconds = maximumBy (comparing snd) $ points rdrs seconds

points :: [Reindeer] -> Int -> [(Reindeer, Int)]
points rdrs kms = foldl (zipWith sumPoints) init $ map (pointsAt rdrs) [1..kms]
  where
    init = map (\r -> (r, 0)) rdrs
    sumPoints (rd, a) (_, b) = (rd, a + b)

pointsAt :: [Reindeer] -> Int -> [(Reindeer, Int)]
pointsAt rdrs seconds = map winnerPoints rmi
  where
    longest = snd $ last $ sortBy (comparing snd) rmi
    rmi = map (\r -> (r, distance r seconds)) rdrs
    winnerPoints (rd, dist)
      | dist == longest = (rd, 1)
      | otherwise       = (rd, 0)

