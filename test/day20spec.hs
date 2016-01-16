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

  describe "Delivers presents" $ do
    it "returns 10 for house # 1" $ do
      elves 1 `shouldBe` 10

