module Day14Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day14

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.
  let dancer = mkRd 27 5 132
  -- Cupid can fly 22 km/s for 2 seconds, but then must rest for 41 seconds.
  let cupid = mkRd 22 2 41
  -- Rudolph can fly 11 km/s for 5 seconds, but then must rest for 48 seconds.
  let rudolph = mkRd 11 5 48
  -- Donner can fly 28 km/s for 5 seconds, but then must rest for 134 seconds.
  let donner = mkRd 28 5 134
  -- Dasher can fly 4 km/s for 16 seconds, but then must rest for 55 seconds.
  let dasher = mkRd 4 16 55
  -- Blitzen can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
  let blitzen = mkRd 14 3 38
  -- Prancer can fly 3 km/s for 21 seconds, but then must rest for 40 seconds.
  let prancer = mkRd 3 21 40
  -- Comet can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
  let comet = mkRd 18 6 103
  -- Vixen can fly 18 km/s for 5 seconds, but then must rest for 84 seconds.
  let vixen = mkRd 18 5 84

  describe "Reindeer" $ do
    it "calculats the distance using the resting time" $ do
      distance comet 1000 `shouldBe` 200 

    it "caculatest the max after " $ do
      longest reindeers 2503 `shouldBe` 1
