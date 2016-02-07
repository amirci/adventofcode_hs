module Day17Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day17

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "storeEggnog" $ do
    
    context "using the sample" $ do
      it "calculates the containers" $ do
        storeEggnog 25 [20, 15, 10, 5, 5] `shouldBe` 4 -- [[10, 15], [5, 20], [5, 20], [5, 5, 15]] 

    context "using the file" $ do
      it "calculates container for 150 liters" $ do
        contents <- readFile "test/day17.input.txt"
        let ctnrs = map (read :: String -> Int) $ lines contents
        storeEggnog 150 ctnrs `shouldBe` 1638

  describe "Minimum containers" $ do
    context "using the file" $ do
      it "calculates minimum containers for 150 liters" $ do
        contents <- readFile "test/day17.input.txt"
        let ctnrs = map (read :: String -> Int) $ lines contents
        storeMin 150 ctnrs `shouldBe` 17

