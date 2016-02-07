module Day16Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day16

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "parse" $ do
    it "parses children" $ do
      let cmp = parse "Sue 201: children: 5, cats: 2"
      children cmp `shouldBe` Just 5
      cats cmp `shouldBe` Just 2
      pomeranians cmp `shouldBe` Nothing

  describe "Finding the aunt" $ do

    let eq = (==) :: Int -> Int -> Bool
    let sample = Compound { children = eq 3
                            , cats        = eq 7
                            , samoyeds    = eq 2
                            , pomeranians = eq 3
                            , akitas      = eq 0
                            , vizslas     = eq 0
                            , goldfish    = eq 5
                            , trees       = eq 3
                            , cars        = eq 2
                            , perfumes    = eq 1
                          }

    context "using equality for the sample" $ do
      it "finds the aunt index from the input file" $ do
        contents <- readFile "test/day16.input.txt"
        let aunts = lines contents
        -- is actually zero based so it is 103
        findAunt aunts sample `shouldBe` Just 102 
        
    context "using the retroencabulator for the sample" $ do
      let sample2 = sample { cats = (> 7)
                            , trees = (> 3)
                            , pomeranians = (< 3)
                            , goldfish = (< 5)
                          }

      it "finds the aunt index from the input file" $ do
        contents <- readFile "test/day16.input.txt"
        let aunts = lines contents
        -- is actually zero based so it is 405
        findAunt aunts sample2 `shouldBe` Just 404 

