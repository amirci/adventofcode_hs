module Day8 where

import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Numeric
import Data.Char

escapeDiff :: String -> Int
escapeDiff s = 2 + length s - length actual
  where actual = escape s

escape2Diff :: String -> Int
escape2Diff s = encodeCount s - length s

escape :: String -> String
escape [] = []
escape ('\\':'\\':xs) = '\\':(escape xs)
escape ('\\':'"':xs) = '"':escape xs
escape ('\\':'x':a:b:xs) = hexChar:(escape xs)
  where hexChar = chr $ fst $ head $ readHex $ a:b:[]
escape (a:xs) = a:(escape xs)

encodeCount :: String -> Int
encodeCount s = (+ 2) $ sum $ map enc s
  where
    enc '\\' = 2
    enc '\"' = 2
    enc _    = 1

