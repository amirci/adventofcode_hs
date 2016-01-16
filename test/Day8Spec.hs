module Day8Spec (main, spec) where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers
import Debug.Trace

import Test.Hspec
import qualified Data.Map as Map

import Day8

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  context "escaping strings" $ do
    describe "Escapes a string" $ do

      it "Returns the same string when nothing to escape" $ do
        escape "abc" `shouldBe` "abc"

      it "Returns single backlash for double" $ do
        escape "aa\\\\a" `shouldBe` "aa\\a"

      it "Returns a double quote when escaped" $ do
        escape "aa\\\"a" `shouldBe` "aa\"a"

      it "Returns a char by hex number" $ do
        escape "aa\\x26" `shouldBe` "aa\x26"

    describe "Calculates the difference of string length and memory represenation" $ do

      it "Returns 0 for empty string" $ do
        escapeDiff "" `shouldBe` 2

      it "Returns 3 for abc" $ do
        escapeDiff "abc" `shouldBe` 2

      it "Counts the escaped double quote as one character" $ do
        escapeDiff "aaa\\\"aaa" `shouldBe` 3

      it "Counts the double backslash as one character" $ do
        escapeDiff "aa\\\\" `shouldBe` 3

      it "Counts the hexa representaiton as one" $ do
        escapeDiff "\\x27" `shouldBe` 5


    describe "Reading form the file" $ do

      it "Returns the difference of chars and representation" $ do
        contents <- readFile "test/day8.input.txt"
        let total = sum $ map escapeDiff $ lines contents
        total `shouldBe` 1350


  context "Encdoding strings" $ do
    describe "Encoding" $ do
      it "Encodes empty string to six characters" $ do
        encode "" `shouldBe` "\"\""

      it "Encodes letters to same letters" $ do
        encode "abc" `shouldBe` "\"abc\""

      it "Encodes double quotes" $ do
        encode "aaa\"aaa" `shouldBe` "\"aaa\\\"aaa\""

      it "Encodes heaxa chars" $ do
        encode "\\x27" `shouldBe` "\"\\\\x27\""

