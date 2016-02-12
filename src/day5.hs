module Day5 where

import Data.List
import Data.Maybe
import Control.Monad

-- It contains at least three vowels (aeiou only), like aei, xazegov, or
-- aeiouaeiouaeiou.  It contains at least one letter that appears twice in a
-- row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).  It does not
-- contain the strings ab, cd, pq, or xy, even if they are part of one of the
-- other requirements.

data Behaviour = Nice | Naughty deriving (Eq, Show)

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) = liftM2 (&&)

niceStr :: String -> Behaviour
niceStr str 
  | isNice str = Nice
  | otherwise = Naughty
  where
    isNice = desirable .&&. threeVowels .&&. doubleLetter
    desirable str = not $ any (flip isInfixOf str) ["ab", "cd", "pq", "xy"]
    threeVowels = (>= 3) . length . filter (flip elem "aeiou")
    doubleLetter = any ((>= 2) . length) . group

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
  where nice = (== Nice) . fn



