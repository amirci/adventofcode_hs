module Day4 where

import Data.List
import Data.Maybe
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BS

mining :: (String->Bool) -> String -> (Int, String)
mining startsWith key = fromJust $ find (startsWith . snd) sequence
  where
    sequence = [(i, hash i) | i <- [1..]]
    hash i = show $ md5 $ BS.pack $ key ++ show i  


swZeroes :: Int -> String -> Bool
swZeroes i = (== zeroes) . fst . (splitAt i)
  where zeroes = replicate i '0'
