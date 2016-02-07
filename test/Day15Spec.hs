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

  let frosting = (4, -2, 0, 0, 5) -- Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
  let candy    = (0, 5, -1, 0, 8) -- Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
  let buts     = (-1, 0, 5, 0, 6) -- Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
  let cinn     = (0, 0, -2, 2, 1) -- Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1
  let igrds = [frosting, candy, buts, cinn]

  describe "Calculates highest cookie score" $ do
    let highest = highest' score

    context "using the example tests" $ do
      -- Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
      -- Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
      let buts = (-1, -2, 6, 3, 8)
      let cinn = (2, 3, -2, -1, 3)

      it "Calculates the highest score" $ do
        highest [buts, cinn] `shouldBe` 62842880

    context "reading ingredients from the file" $ do
      it "Calculates the highest score" $ do
        highest igrds `shouldBe` 18965440

  describe "Highest score for 500 calories" $ do
    let highest = highest' scoreCal

    context "using the sample" $ do
      let buts = (-1, -2, 6, 3, 8)
      let cinn = (2, 3, -2, -1, 3)

      it "Calculates the highest score" $ do
        highest [buts, cinn] `shouldBe` 57600000

    context "reading ingredients from the file" $ do
      it "Calculates the highest score" $ do
        highest igrds `shouldBe` 15862900

