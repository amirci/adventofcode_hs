module Day14 where

type Reindeer = (Int, Int, Int)

speed :: Reindeer -> Int
speed (a, _, _) = a

fly :: Reindeer -> Int
fly (_, b, _) = b

rest :: Reindeer -> Int
rest (_, _, c) = c

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
  
