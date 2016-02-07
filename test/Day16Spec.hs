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

    it "finds nothing" $ do
      let aunts = []
      let compound = empty
      findAunt aunts compound `shouldBe` Nothing

    it "finds the aunt index from the input file" $ do
      contents <- readFile "test/day16.input.txt"
      let aunts = lines contents
      let compound = Compound { children= Just 3
                                , cats= Just 7
                                , samoyeds= Just 2
                                , pomeranians= Just 3
                                , akitas= Just 0
                                , vizslas= Just 0
                                , goldfish= Just 5
                                , trees= Just 3
                                , cars= Just 2
                                , perfumes= Just 1
                              }
      findAunt aunts compound `shouldBe` Just 3
