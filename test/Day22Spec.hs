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

  context "Part A - least amount of mana to win" $ do

    describe "playing game" $ do
      -- Player turn 
      -- - Player has 10 hit points, 0 armor, 250 mana
      -- - Boss has 14 hit points
      -- Player casts Recharge.
      it "casts Recharge" $ do
        let me = Player 10 0 250 0
        let boss = Boss 13 8 
        let poison = spell "Poison"
        let round1@(m1, b1) = cast (me, boss) $ poison
        m1 `shouldBe` me {mana=250-cost poison, spent=cost poison}
        b1 `shouldBe` boss


    --context "using input file" $ do
    --  let boss = Boss { bp=55, damage=8 }
    --  let me   = Player { hp=50, mana=500, armor=0 }

    --  it "finds the least amount of mana to win" $ do
    --    
