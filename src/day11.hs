module Day11 where

import Data.List
import Data.Maybe
import Control.Monad

changePwd :: String -> String
changePwd str = fromJust $ find validPwd $ passwords
  where passwords = drop 1 $ iterate succPwd str

succPwd :: String -> String
succPwd = reverse . succ' . reverse

succ' :: String -> String
succ' ('z':xs) = ('a':succ' xs)
succ' (a:xs)   = (succ a:xs)
succ' []       = []

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) = liftM2 (&&)

validPwd :: String -> Bool
validPwd = straight .&&. pairCount .&&. validChars
  where
    validChars s = not $ any (flip elem s) "iol"
    straight s = any succ3 $ zip3 s (drop 1 s) (drop 2 s)
      where succ3 (a, b, c) = (b == succ a) && (c == succ b)
    pairCount s = (> 1) . length $ nub $ filter samePair $ zip s (drop 1 s)
      where samePair (a, b) = a == b

