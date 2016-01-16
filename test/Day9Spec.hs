module Day9Spec (main, spec) where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers
import Debug.Trace

import Test.Hspec
import qualified Data.Map as Map

import Day9

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "shortest path" $ do
    -- Dublin -> London -> Belfast = 982
    -- London -> Dublin -> Belfast = 605
    -- London -> Belfast -> Dublin = 659
    -- Dublin -> Belfast -> London = 659
    -- Belfast -> Dublin -> London = 605
    -- Belfast -> London -> Dublin = 982
    -- The shortest of these is London -> Dublin -> Belfast = 605

    it "Returns 11 for 1" $ do
      let paths = [ mkPath "London to Dublin = 464"
                  , mkPath "London to Belfast = 518"
                  , mkPath "Dublin to Belfast = 141"
                  ]
      shortest paths `shouldBe` 605

    it "reads from the input file" $ do
      contents <- readFile "test/day9.input.txt"
      let cities = map mkPath $ lines contents
      shortest cities `shouldBe` 207

