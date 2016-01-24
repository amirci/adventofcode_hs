{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import Text.Regex
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Scientific

countJSON:: String -> Double
countJSON [] = 0.0
countJSON str = process $ matchRegexAll aNumber str 
  where
    aNumber = mkRegex "(-?[0-9]+)"
    process Nothing = 0.0
    process (Just (_, n, rest, _)) = (read n :: Double) + countJSON rest

ignoreRed :: String -> Int
ignoreRed str = parse $ fromJust $ decode $ BS.pack str
  where
    parse (Object hm)
      | elem "red" $ HM.elems hm = 0
      | otherwise  = sum $ map parse $ HM.elems hm
    parse (Array val)  = sum $ V.map parse val
    parse (Number num) = fromJust $ toBoundedInteger num
    parse _ = 0

