module Day24Spec (main, spec) where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers
import Debug.Trace

import Test.Hspec
import qualified Data.Map as Map

import Day24

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  context "Part A - Even amount of presents" $ do
    let presents = [1..5] ++ [7..11]

    describe "using the sample" $ do
      it "calculates the minimum quantum" $ do
        minQ presents 3 `shouldBe` 99

    describe "using the input file" $ do
      it "calculates the minimum quantum" $ do
        contents <- readFile "test/day24.input.txt"
        let presents = map (read::String->Int) $ lines contents
        minQ presents 3 `shouldBe` 11266889531

  context "Part B - Four sections (including trunk)" $ do
    let presents = [1..5] ++ [7..11]

    describe "using the sample" $ do
      it "calculates the minimum quantum" $ do
        minQ presents 4 `shouldBe` 44

    describe "using the input file" $ do
      it "calculates the minimum quantum" $ do
        contents <- readFile "test/day24.input.txt"
        let presents = map (read::String->Int) $ lines contents
        minQ presents 4 `shouldBe` 77387711

