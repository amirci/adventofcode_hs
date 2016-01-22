module Day11 where

import Data.List
import Data.Maybe

changePwd :: String -> String
changePwd str = fromJust $ find validPwd $ passwords
  where passwords = iterate succPwd $ succPwd str

succPwd :: String -> String
succPwd str = reverse $ succ' $ reverse str
  where
    succ' ('z':xs) = ('a':succ' xs)
    succ' (a:xs)   = (succ a:xs)
    succ' []       = []

validPwd :: String -> Bool
validPwd str = and $ map ($ str) [straight, (> 1) . pairCount, valid]
  where
    valid s = not $ any (\c -> elem c s) "iol"
    straight (a:b:c:xs) = (succ a == b && succ b == c) || straight (b:c:xs)
    straight _ = False
    pairCount (a:b:xs) 
      | a == b    = 1 + pairCount xs
      | otherwise = pairCount (b:xs)
    pairCount _ = 0
