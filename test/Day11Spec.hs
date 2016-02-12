module Day11Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day11

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Validating passwords" $ do
    it "returns valid" $ do
      validPwd "ghjaabcc" `shouldBe` True

    it "returns invalid because has an i" $ do
      validPwd "hijklmmn" `shouldBe` False

  describe "Calculate the next password" $ do
    it "returns abcdffaa for abcdefgh" $ do
      changePwd "abcdefgh" `shouldBe` "abcdffaa"

    it "returns ghjaabcc because skips ghi" $ do
      changePwd "ghijklmn" `shouldBe` "ghjaabcc"

    it "returns after two iterations" $ do
      let two = iterate changePwd "cqjxjnds" !! 2
      two `shouldBe` "cqkaabcc"

    it "returns for the puzzle input" $ do
      changePwd "cqjxjnds" `shouldBe` "cqjxxyzz"


