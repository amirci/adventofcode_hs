module Day21Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day21

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let myself = Player { hp=100, damp=0, armorp=0 }
  let other  = Player { hp=104, damp=8, armorp=1 }

  describe "minimum gold winnineg" $ do
    it "calculates the min gold to win" $ do
      minGold myself other `shouldBe` 78

  describe "maximum gold loosing" $ do
    it "calculates the max gold when loosing" $ do
      maxGold myself other `shouldBe` 148

