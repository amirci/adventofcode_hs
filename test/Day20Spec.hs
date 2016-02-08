module Day20Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day20

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

-- House 1 got 10 presents.
-- House 2 got 30 presents.
-- House 3 got 40 presents.
-- House 4 got 70 presents.
-- House 5 got 60 presents.
-- House 6 got 120 presents.
-- House 7 got 80 presents.
-- House 8 got 150 presents.
-- House 9 got 130 presents.

  context "using the sample" $ do
    it "returns 10 for house # 1" $ do
      presents !! 0 `shouldBe` 10
    it "returns 70 for house # 4" $ do
      presents !! 3 `shouldBe` 70
    it "returns 130 for house # 9" $ do
      presents !! 8 `shouldBe` 130

  context "using the problem input" $ do

    it "returns the lowest house for 33100000" $ do
      -- part1 33100000 `shouldBe` 776160
      lowestHouse 33100000 `shouldBe` 776160

