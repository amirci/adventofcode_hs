module Day12 where

import Text.Regex

countJSON:: String -> Double
countJSON [] = 0.0
countJSON str = process $ matchRegexAll aNumber str 
  where
    aNumber = mkRegex "(-?[0-9]+)"
    process Nothing = 0.0
    process (Just (_, n, rest, _)) = (read n :: Double) + countJSON rest


ignoreRed :: String -> Double
ignoreRed str = countJSON replaced
  where
    replaced = subRegex (mkRegex "{[^{]*\"red\"[^}]*}") str "false"
