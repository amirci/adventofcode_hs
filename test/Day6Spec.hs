module Day6Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers
import Debug.Trace

import Test.Hspec
import qualified Data.Set as Set

import Day6

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let emptyBoard = Set.empty
  let light = (10, 10)
  let board = Set.singleton (10, 10)  

  describe "Turning on lights" $ do
    it ("Turns on " ++ show light) $ do
      turnOn light emptyBoard `shouldSatisfy` isOn light

  describe "Turning off lights" $ do
    context "When the light is on" $ do
      it ("Turns off " ++ show light) $ do
        turnOff light board `shouldSatisfy` isOff light
    context "When the light is off" $ do
      it "Still off" $ do
        turnOff light emptyBoard `shouldSatisfy` isOff light


  describe "Toggle lights" $ do
    context "when the light is on" $ do
      it "turns it off" $ do
        toggle light board `shouldSatisfy` isOff light
    
    context "when the light is off" $ do
      it "turns it on" $ do
        toggle light emptyBoard `shouldSatisfy` isOn light


  describe "Parsing instruction" $  do
    let coords = [(i,j) | i<-[10..20], j<-[10..20]]
    context "when the instruction is turn on" $ do
      it "turns on the lights in range" $ do
        let actual = command "turn on 10,10 through 20,20" emptyBoard
        let lights = map (flip isOn $ actual) coords 
        and lights `shouldBe` True
        lightCount actual `shouldBe` 121

    context "when the instruction is turn off" $ do
      it "turns off the lights in range" $ do
        let board = command "turn on 10,10 through 20,20" emptyBoard
        let actual = command "turn off 10,10 through 20,20" board
        let lights = map (flip isOff $ actual) coords 
        and lights `shouldBe` True
        lightCount actual `shouldBe` 0

    context "toggle first line" $ do
      it "turns on first line" $ do
        let actual = command "toggle 0,0 through 999,0" emptyBoard
        lightCount actual `shouldBe` 1000

    context "Turn off middle" $ do
      it "turns off middle" $ do
        let board = command "turn on 490,490 through 510,510" emptyBoard
        let actual = command "turn off 499,499 through 500,500" board
        lightCount actual `shouldBe` 437


--   describe "Reading instructions from file" $ do
--     it "Reads all the instructions" $ do
--       contents <- readFile "test/day6.input.txt"
--       let count = lightCount $ foldl (flip command) emptyBoard $ lines contents
--       count `shouldBe` 400410
