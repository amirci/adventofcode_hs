module Day22Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day22

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "playing game" $ do

    let myself = Player { hp=100, armor=0 }
    let boss   = Player { hp=55 , armor=0, dmg=8 }

    it "plays a round" $ do
      play myself boss [] `shouldBe` (1, 0)
