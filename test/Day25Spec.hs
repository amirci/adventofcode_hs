module Day25Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day25

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  context "Part A" $ do
    describe "newCode" $ do
      let codes' = codes 20151125
      it "returns the second" $ codes' !! 1 `shouldBe` 31916031
      it "returns the third"  $ codes' !! 2 `shouldBe` 18749137
        
    describe "nth" $ do
      it "returns the 12th number" $ nth 4 2 `shouldBe` 12
      it "returns the 15th number" $ nth 5 2 `shouldBe` 17

    describe "using the input file" $ do
      let codes' = codes 20151125

      it "finds the code at row 2978 and col 3083" $ do
        let pos = nth 2978 3083 - 1
        codes' !! pos `shouldBe` 2650453
