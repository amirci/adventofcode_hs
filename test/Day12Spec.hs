module Day12Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day12

main :: IO ()
main = hspec spec

spec :: Spec
spec =do

  describe "Using some strings" $ do
    it "returns the sum of the array" $ do
      countJSON "[1,2,3]" `shouldBe` 6

    it "returns the sum of the object" $ do
      countJSON "{a:2, b:4}" `shouldBe` 6

    it "returns the sum of nested arrays" $ do
      countJSON "[[[3]]]" `shouldBe` 3

    it "returns the sum of object with negatives" $ do
      countJSON "{\"a\":{\"b\":4},\"c\":-1}" `shouldBe` 3

    it "returns zero" $ do
      countJSON "{\"a\":[-1, 1]}" `shouldBe` 0

    it "sums all the numbers from the JSON file" $ do
      contents <- readFile "test/day12.input.txt"
      countJSON contents `shouldBe` 156366

  describe "Ignoring read" $ do
    it "ignores the red object" $ do
      ignoreRed "[1,{\"c\":\"red\",\"b\":2},3]" `shouldBe` 4

    it "ignores the red object" $ do
      ignoreRed "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}" `shouldBe` 0

    it "red in the array has no effect" $ do
      ignoreRed "[1,\"red\",5]" `shouldBe` 6

    it "matches the smallest object that has red" $ do
      ignoreRed "{\"e\":{\"e\":161,\"a\":\"blue\",\"d\":{\"e\":-14,\"a\":\"red\"}}}" `shouldBe` 161

    it "sums all the numbers from the JSON file" $ do
      contents <- readFile "test/day12.input.txt"
      ignoreRed contents `shouldBe` 156366

