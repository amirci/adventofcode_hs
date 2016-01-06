module Day7Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers
import Debug.Trace

import Test.Hspec
import qualified Data.Map as Map

import Day7

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Simple circuit" $ do
    let inst = [ "123 -> x"
                , "456 -> y"
                , "x AND y -> d"
                , "x OR y -> e"
                , "x LSHIFT 2 -> f"
                , "y RSHIFT 2 -> g"
                , "NOT x -> h"
                , "NOT y -> i" 
                ]

    let board = foldl (flip addCircuit) mkBoard inst

    let expects = [("h", 65412), ("i", 65079), ("g", 114),("f", 492),("d", 72),("e", 507),("x", 123), ("y", 456)]

    it "Assigns all circuits" $ do
      let (expected, actual) = unzip $ map (\(c, v) -> ((c, Just v), (c, wire c board))) expects
      actual `shouldBe` expected

  describe "Delayed circuit" $ do
    let inst = [ "lx -> a", "456 -> lx"]

    let board = foldl (flip addCircuit) mkBoard inst

    it "Assigns a after is set" $ do
      wire "a" board `shouldBe` Just 456

  describe "Circuit from file" $ do
    it "Calculates the a wire" $ do
      contents <- readFile "test/day7.input.txt"
      let board = foldl (flip addCircuit) mkBoard $ lines contents
      wire "a" board `shouldBe` Just 46065

