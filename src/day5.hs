module Day5 where

import Data.List
import Data.Maybe
import Debug.Trace

-- It contains at least three vowels (aeiou only), like aei, xazegov, or
-- aeiouaeiouaeiou.  It contains at least one letter that appears twice in a
-- row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).  It does not
-- contain the strings ab, cd, pq, or xy, even if they are part of one of the
-- other requirements.

data Behaviour = Nice | Naughty deriving (Eq, Show)

niceStr :: String -> Behaviour
niceStr str 
  | niceRules = Nice
  | otherwise = Naughty
  where
    niceRules = not rejected && atLeast3Vowels && doubleLetter
    rejected = any (\s -> isInfixOf s str) undesirable
    undesirable = ["ab", "cd", "pq", "xy"]
    atLeast3Vowels = (>= 3) $ length $ filter isVowel str
    doubleLetter = any (>= 2) $ map length $ group str
    isVowel c = elem c "aeiou"

-- It contains a pair of any two letters that appears at least twice in the
-- string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like
-- aaa (aa, but it overlaps).
-- It contains at least one letter which repeats with exactly one letter
-- between them, like xyx, abcdefeghi (efe), or even aaa.
niceStr2 :: String -> Behaviour
niceStr2 str
  | twicePair && letterRepeat = Nice
  | otherwise = Naughty
  where
    gather n = map (take n) . dropLast n . tails
      where dropLast n xs = zipWith const xs (drop n xs)

    twicePair = not $ null $ filter twiceOrMore $ group pairs
      where 
        twiceOrMore = (>= 2) . length
        pairs = sort $ gather 2 $ shrunk
        shrunk = concat $ map repeated $ group str
        repeated s = if (length s) == 3 then take 2 s else s

    letterRepeat = not $ null $ filter sameBeginAndEnd triplets
      where
        sameBeginAndEnd x = head x == last x
        triplets = gather 3 str 

countNice :: (String->Behaviour) -> String -> IO Int
countNice fn fileName = do
  content <- readFile fileName
  return $ length $ filter nice $ lines content
  where 
    nice = (== Nice) . fn
    -- printIt s = trace ("-- " ++ s ++ " " ++ (tos $ fn s)) (nice s)



