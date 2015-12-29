module Day3Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day3

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "When Santa is alone" $ do
    it "visits only one house for one instruction" $ do
      santaHouses ">" `shouldBe` 2

    it "visits 3 houses for a square" $ do
      santaHouses "^>V<" `shouldBe` 3

    it "Visits houses form a file" $ do
      contents <- readFile "test/day3.input.txt"
      santaHouses contents `shouldBe` 2081

  describe "With Robot-Santa" $ do
    it "Visits 3 houses with one instruction" $ do
      robotHouses "^v" `shouldBe` 3

    it "Visits 3 houses when coming back to the same house" $ do
      robotHouses "^>v<" `shouldBe` 3

    it "Visits four houses when coming back to the same house" $ do
      robotHouses "^>^<v^" `shouldBe` 4

    it "Visits 11 houses in different directions" $ do
      robotHouses "^v^v^v^v^v" `shouldBe` 11

    it "Visits houses form a file" $ do
      contents <- readFile "test/day3.input.txt"
      robotHouses contents `shouldBe` 2341

