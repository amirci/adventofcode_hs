module Day18Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day18

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "part A" $ do
    context "using the sample scenario" $ do
      let board = mkBoard [".#.#.#" ,"...##." ,"#....#" ,"..#..." ,"#.#..#" ,"####.." ]

      let step1 = mkBoard [ "..##.."
                          , "..##.#"
                          , "...##."
                          , "......"
                          , "#....."
                          , "#.##.."]

      it "evolves to the first step" $ do
        evolve board `shouldBe` step1

      let step4 = mkBoard ["......"
                        ,"......"
                        ,"..##.."
                        ,"..##.."
                        ,"......"
                        ,"......"
                      ]

      it "evolves 4 times " $ do
        let evolution = head $ drop 4 $ iterate evolve board
        evolution `shouldBe` step4

      it "evolves xxx" $ do
        let board = mkBoard ["####.#"
                            ,".##..."
                            ,"###.##"
                            ,"....#." ]

        let exp   = mkBoard ["#..#.." 
                            ,".....#"
                            ,"#.#.##"
                            ,".#.###" ]

        evolve board `shouldBe` exp

      context "reading from the file" $ do
        it "Has 768 lights after 100 evolutions" $ do
          contents <- readFile "test/day18.input.txt"
          let board = mkBoard $ lines contents
          let evolution = (iterate evolve board) !! 100
          lightCount evolution `shouldBe` 768

