module Day10Spec (main, spec) where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers
import Debug.Trace

import Test.Hspec
import qualified Data.Map as Map

import Day10

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "counting numbers" $ do

    it "Returns 11 for 1" $ do
      numCount "1" `shouldBe` "11"

    it "Returns 111221 for 312211" $ do
      numCount "111221" `shouldBe` "312211"

    it "Returns xx for the input 3113322113" $ do
      numCount "3113322113" `shouldBe` "132123222113"

    it "Returns xx after 40 times" $ do
      let fortyTimes = foldr (.) id (replicate 40 numCount)
      (length $ fortyTimes "3113322113") `shouldBe` 329356


    it "Returns xx after 50 times" $ do
      let fiftyTimes = foldr (.) id (replicate 50 numCount)
      (length $ fiftyTimes "3113322113") `shouldBe` 4666278
