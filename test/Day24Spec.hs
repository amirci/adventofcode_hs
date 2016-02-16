module Day24Spec (main, spec) where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers
import Debug.Trace

import Test.Hspec
import qualified Data.Map as Map

import Day24

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  context "Part A - Even amount of presents" $ do
    let presents = [1..5] ++ [7..11]

    describe "using the sample" $ do

      it "calculates the first" $ do
        let (len, qtum) = groups presents
        len `shouldBe` 2
