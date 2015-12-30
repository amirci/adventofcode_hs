module Day5Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day5

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Second set of rules" $ do
    context "For nice strings" $ do
      it "Returns nice because repeats qj and zxz" $ do
        niceStr2 "qjhvhtzxzqqjkmpb" `shouldBe` Nice

      it "Returns nice because repeats xx and xyx" $ do
        niceStr2 "xxyxx" `shouldBe` Nice

      it "Reeturns nice because xex and wwww" $ do
        niceStr2 "rxexcbwhiywwwwnu" `shouldBe` Nice

    context "For naughty strings" $ do
      it "Returns naughty because has a pair tg but no letter repeat" $ do
        niceStr2 "uurcxstgmygtbstg" `shouldBe` Naughty

      it "Returns naughty because the pair overlaps in aaa" $ do
        niceStr2 "aaa" `shouldBe` Naughty

      it "Returns naughty because it has odo but no repeating pair" $ do
        niceStr2 "ieodomkazucvgmuy" `shouldBe` Naughty

    context "From a file" $ do
      it "Returns the count" $ do
        countNice niceStr2 "test/day5.input.txt" `shouldReturn` 51

  describe "First set of rules" $ do
    context "Nice strings" $ do
      it "Returns Nice because is the same vowel 3 times" $ do
        niceStr "aaa" `shouldBe` Nice

      it "Returns Nice because has double letters and at least 3 vowels" $ do
        niceStr "ugknbfddgicrmopn" `shouldBe` Nice

    context "From a file" $ do
      it "Returns the count" $ do
        countNice niceStr "test/day5.input.txt" `shouldReturn` 236

    context "Naughty strings" $ do
      it "Because no double letter" $ do
        niceStr "jchzalrnumimnmhp" `shouldBe` Naughty

      it "Because xy" $ do
        niceStr "haegwjzuvuyypxyu" `shouldBe` Naughty

      it "Because only one vowel" $ do
        niceStr "dvszwmarrgswjxmb" `shouldBe` Naughty

