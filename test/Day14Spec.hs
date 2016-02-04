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
  let cupid   = mkRd 22 2 41  -- Cupid can fly 22 km/s for 2 seconds, but then must rest for 41 seconds.
  let rudolph = mkRd 11 5 48  -- Rudolph can fly 11 km/s for 5 seconds, but then must rest for 48 seconds.
  let donner  = mkRd 28 5 134 -- Donner can fly 28 km/s for 5 seconds, but then must rest for 134 seconds.
  let dasher  = mkRd 4 16 55  -- Dasher can fly 4 km/s for 16 seconds, but then must rest for 55 seconds.
  let blitzen = mkRd 14 3 38  -- Blitzen can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
  let prancer = mkRd 3 21 40  -- Prancer can fly 3 km/s for 21 seconds, but then must rest for 40 seconds.
  let comet   = mkRd 18 6 103 -- Comet can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
  let dancer  = mkRd 27 5 132 -- Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.
  let vixen   = mkRd 18 5 84  -- Vixen can fly 18 km/s for 5 seconds, but then must rest for 84 seconds.

  let reindeers = [vixen, comet, prancer, blitzen, dasher, donner, rudolph, cupid, dancer]

  context "winning points calculations" $ do
    it "returns the winner after 1000 seconds" $ do
      let comet = mkRd 14 10 127
      let dancer = mkRd 16 11 162
      points [dancer, comet] 300 `shouldBe` [(dancer, 239), (comet, 61)]
      points [dancer, comet] 400 `shouldBe` [(dancer, 289), (comet, 111)]
      points [dancer, comet] 500 `shouldBe` [(dancer, 307), (comet, 193)]

    -- it "returns the winner after 2503 seconds" $ do
    --  winner reindeers 2503 `shouldBe` (donner, 1072)

  context "distance calculations" $ do
    describe "Calculates the distance" $ do
      it "calculats that can fly 9 times" $ do
        distance comet 1000 `shouldBe` 1080
      
      it "calculats that can fly 9 times" $ do
        distance vixen 1000 `shouldBe` 1080

    describe "Calculates the longest" $ do
      it "caculatest the max after " $ do
        longest reindeers 2503 `shouldBe` 2640
