module Day23Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day23

instance Arbitrary Computer where
  arbitrary = do
    Positive x <- arbitrary
    Positive y <- arbitrary
    return $ Computer x y

--instance (Arbitrary Computer, Arbirary Int) where
--  arbitrary = do
--    Positive ptr <- arbitrary
--    cpt <- arbitrary
--    return (cptr, ptr)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let cpt = bootUp

  context "Part A" $ do
    let parse' = flip parse $ (cpt, 0)

    describe "parse" $ do
      context "inc command" $ do
        it "inc a" $ parse' "inc a" `shouldBe` (Computer 1 0, 1)
        it "inc b" $ parse' "inc b" `shouldBe` (Computer 0 1, 1)

      context "hlf command" $ do
        let parse' = flip parse $ (Computer 4 4, 0)
        it "hlf a" $ parse' "hlf a" `shouldBe` (Computer 2 4, 1)
        it "hlf b" $ parse' "hlf b" `shouldBe` (Computer 4 2, 1)

      context "tpl command" $ do
        prop "Triples the value of a" $ 
          \(cpt@Computer {rega=ra}, Positive ptr) -> (parse "tpl a" $ (cpt, ptr)) == (cpt {rega=rega cpt * 3}, ptr + 1)

        prop "Triples the value of b" $ 
          \(cpt@Computer {regb=rb}, Positive ptr) -> (parse "tpl b" $ (cpt, ptr)) == (cpt {regb=regb cpt * 3}, ptr + 1)

      context "jump command" $ do
        prop "jumps an offset" $ 
          \(Positive x) -> (parse' ("jmp +" ++ show x)) == (cpt, x)

      context "jump if even command" $ do
        let parse' = flip parse $ (Computer 2 1, 0)
        it "jumps 2 because a=2" $ parse' "jie a, +2" `shouldBe` (Computer 2 1, 2)

      context "jump if one command" $ do
        let parse' = flip parse $ (Computer 1 1, 0)
        it "jumps 2 because a=1" $ parse' "jio a, +2" `shouldBe` (Computer 1 1, 2)

    describe "Using the sample commands" $ do
      let cmds = ["inc a", "jio a, +2", "tpl a", "inc a"]

      it "increments only register a" $ do
        let cmp = run cmds bootUp
        rega cmp `shouldBe` 2

    describe "Using the sample file" $ do

      it "applies all the commands" $ do
        contents <- readFile "test/day23.input.txt"
        let cmds = lines contents
        let cpt = run cmds bootUp
        cpt `shouldBe` (Computer 1 255)

  context "part B" $ do
    describe "Sample file starting with rega = 1" $ do
      it "applies all the commands" $ do
        contents <- readFile "test/day23.input.txt"
        let cmds = lines contents
        let cpt = run cmds $ bootUp {rega=1}
        cpt `shouldBe` (Computer 1 334)
