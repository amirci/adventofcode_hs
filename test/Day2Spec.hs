module Day2Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Wrapping the presents" $ do
    it "Calculates" $ do
      paperWrap (2, 3, 4) `shouldBe` 58
      paperWrap (1, 1, 10) `shouldBe` 43


    it "Wraps all the presents from the file" $ do
      (fromFile paperWrap "test/day2.input.txt") `shouldReturn` 1598415


  describe "Ribbon calculation" $ do
    it "returns 34 for 2x3x4" $ do
      ribbon (2, 3, 4) `shouldBe` 34
    it "returns 14 for 1x1x10" $ do
      ribbon (1, 1, 10) `shouldBe` 14
    it "Puts a ribbon in all the presents from the file" $ do
      (fromFile ribbon "test/day2.input.txt") `shouldReturn` 3812909

