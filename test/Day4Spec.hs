module Day4Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day4

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let swz5 = swZeroes 5
  let swz6 = swZeroes 6
  let mining5 = mining swz5
  let mining6 = mining swz6
  let minHashAt num swz (i, h) = num == i && swz h

  describe "Mining adventcoins with 5 zeroes" $ do
    it "Returns 609043 for abcdef key" $ do
      mining5 "abcdef" `shouldSatisfy` minHashAt 609043 swz5
    
    it "Returns 1048970 for pqrstuv key" $ do
      mining5 "pqrstuv" `shouldSatisfy` minHashAt 1048970 swz5 

    it "Returns 254575 for bgvyzdsv key" $ do
      mining5 "bgvyzdsv" `shouldSatisfy` minHashAt 254575 swz5

  describe "Mining adventcoins with 6 zeroes" $ do
    it "Returns 1038736 for bgvyzdsv key" $ do
      mining6 "bgvyzdsv" `shouldSatisfy` minHashAt 1038736 swz6

