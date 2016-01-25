module Day13Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers
import qualified Data.MultiMap as MM

import Test.Hspec

import Day13

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  -- Alice would gain 54 happiness units by sitting next to Bob.
  -- Alice would lose 79 happiness units by sitting next to Carol.
  -- Alice would lose 2 happiness units by sitting next to David.
  -- Bob would gain 83 happiness units by sitting next to Alice.
  -- Bob would lose 7 happiness units by sitting next to Carol.
  -- Bob would lose 63 happiness units by sitting next to David.
  -- Carol would lose 62 happiness units by sitting next to Alice.
  -- Carol would gain 60 happiness units by sitting next to Bob.
  -- Carol would gain 55 happiness units by sitting next to David.
  -- David would gain 46 happiness units by sitting next to Alice.
  -- David would lose 7 happiness units by sitting next to Bob.
  -- David would gain 41 happiness units by sitting next to Carol.
  describe "Seating example" $ do
    let alice = "Alice"
    let bob = "Bob"
    let carol = "Carol"
    let david = "David"
    let requirements = MM.fromList [
                          (alice, (54, bob)), (alice, (-79, carol)), (alice, (-2, david))
                          , (bob, (83, alice)), (bob, (-7, carol)), (bob, (-63, david))
                          , (carol, (-62, alice)), (carol, (60, bob)), (carol, (55, david))
                          , (david, (46, alice)), (david, (-7, bob)), (david, (41, carol))
                        ]

    it "maximizes the happiness difference" $ do
      maxHappiness requirements `shouldBe` 330


  describe "Parsing gains and loses" $ do

    it "parses the gains of a person" $ do
      parse "Bob would gain 40 happiness units by sitting next to Alice." `shouldBe` ("Bob", (40, "Alice"))

    it "parses the loses of a person" $ do
      parse "Bob would lose 40 happiness units by sitting next to Alice." `shouldBe` ("Bob", (-40, "Alice"))

  describe "Reading form the source input file" $ do

    it "calculates maximum points for" $ do
      contents <- readFile "test/day13.input.txt"
      let req = MM.fromList $ map parse $ lines contents
      maxHappiness req `shouldBe` 733
  
    it "calculates maximum points including myself" $ do
      contents <- readFile "test/day13.input.txt"
      let req = MM.fromList $ map parse $ lines contents
      let people = MM.keys $ req
      let req2 = foldl (\mm p -> MM.insert "Me" (0, p) $ MM.insert p (0, "Me") mm) req people
      maxHappiness req2 `shouldBe` 725
    
