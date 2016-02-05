module Day15Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day15

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Calculates highest cookie score" $ do

    context "using the example tests" $ do
      -- Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
      -- Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
      let buts = (-1, -2, 6, 3)
      let cinn = (2, 3, -2, -1)

      it "Calculates the highest score" $ do
        highest [buts, cinn] `shouldBe` 62842880

    context "reading ingredients from the file" $ do
      let frosting = (4, -2, 0, 0) -- Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
      let candy    = (0, 5, -1, 0) -- Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
      let buts     = (-1, 0, 5, 0) -- Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
      let cinn     = (0, 0, -2, 2) -- Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1
      let igrds = [frosting, candy, buts, cinn]

      it "Calculates the highest score" $ do
        highest igrds `shouldBe` 1
