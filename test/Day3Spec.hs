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

  describe "Delivers presents to houses" $ do
    it "returns one only for one instruction" $ do
      countPresents ">" `shouldBe` 2

    it "returns 3 for a square" $ do
      countPresents "^>V<" `shouldBe` 3

  describe "Reading from a file" $ do
    it "Calculates the path" $ do
      (fromFile "test/day3.input.txt") `shouldReturn` 2081
