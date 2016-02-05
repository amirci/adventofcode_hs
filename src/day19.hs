module Day19 where

import qualified Data.MultiMap as MMap
import Text.Regex
import Data.List
import qualified Data.Text as T
import Data.Char

type Replacements = MMap.MultiMap String String
type Molecule = String

calibrate :: String -> Replacements -> [Molecule]
calibrate input replacements = nub $ MMap.foldlWithKey createMolecules [] replacements
  where
    createMolecules acc key val = acc ++ (replaceAll input key val)

replaceAll :: String -> String -> String -> [Molecule]
replaceAll input key val = replace "" $ findMatch input
  where
    regex = mkRegex $ "(" ++ key ++ ")"
    findMatch = matchRegexAll regex
    replace _ Nothing = []
    replace acc (Just(before, match, after, _)) = visited ++ nextMatch
      where 
        visited = [acc ++ before ++ val ++ after]
        nextMatch = replace (acc ++ before ++ match) $ findMatch after

-- Solution from https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4h7ji
steps :: String -> Int
steps input = symbolCount - count "Rn" - count "Ar" - 2 * count "Y" - 1
  where
    symbolCount = length $ filter isUpper input
    count s = T.count (T.pack s) (T.pack input)
  
