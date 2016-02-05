module Day19Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day19
import qualified Data.MultiMap as MMap
import Data.List.Split

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Machine molecule calibration" $ do

    context "using the sample" $ do
      let molecule = "HOH"
      let replacements = MMap.fromList [("H", "HO"), ("H", "OH"), ("O", "HH")]
      it "calibrates to 4 distinct molecules" $ do
        calibrate molecule replacements `shouldBe` ["HOOH", "HOHO", "OHOH", "HHHH"]

      it "replaces all the Hs" $ do
        replaceAll molecule "H" "HO" `shouldBe` ["HOOH", "HOHO"]

    context "reading from the file" $ do
      let molecule = "ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF"

      it "counts the steps to create the input" $ do
        steps molecule `shouldBe` 420

      it "creates all the molecules" $ do
        contents <- readFile "test/day19.input.txt"
        let replacements = MMap.fromList $ map ((\xs -> (head xs, last xs)) . splitOn " ") $ lines contents
        let count = length . calibrate molecule

        count replacements `shouldBe` 576


