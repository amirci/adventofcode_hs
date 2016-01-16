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

escape :: String -> String
escape [] = []
escape ('\\':'\\':xs) = '\\':(escape xs)
escape ('\\':'"':xs) = '"':escape xs
escape ('\\':'x':a:b:xs) = hexChar:(escape xs)
  where hexChar = chr $ fst $ head $ readHex $ a:b:[]
escape (a:xs) = a:(escape xs)

encode :: String -> String
encode s = "\"" ++ encode' s ++ "\""
  where
    encode' [] = []
    encode' ('\\':xs) = "\\\\" ++ encode' xs
    encode' ('\"':xs) = "\\\"" ++ encode' xs
    encode' (a:xs) = a:(encode' xs)
