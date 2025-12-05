module Days.Dec1Spec where

import Days.Dec1
import Test.Hspec
import Util

dial :: Dial
dial = Dial 50 0 0

spec :: Spec
spec = do
    describe "doInstruction" $ do
        it "can turn dial left" $
            doInstruction (Instruction 'L' 1) dial `shouldBe` Dial 49 0 0
        it "can turn dial right" $
            doInstruction (Instruction 'R' 1) dial `shouldBe` Dial 51 0 0
        it "captures a single left final zero" $
            doInstruction (Instruction 'L' 50) dial `shouldBe` Dial 0 1 1
        it "captures left running zeros" $
            doInstruction (Instruction 'L' 550) dial `shouldBe` Dial 0 1 6
        it "captures a right final zero" $
            doInstruction (Instruction 'R' 50) dial `shouldBe` Dial 0 1 1
        it "captures right running zeros" $
            doInstruction (Instruction 'R' 550) dial `shouldBe` Dial 0 1 6
    describe "exampleInput" $ do
        input <- runIO (getExampleInput "dec1.txt")
        let instructions = map parseInstruction input
        let dial' = doInstructions instructions dial
        it "finds correct pt. 1 result" $
            finalZero dial' `shouldBe` 3
        it "finds correct pt. 2 result" $
            runningZero dial' `shouldBe` 6
    describe "puzzleInput" $ do
        input <- runIO (getPuzzleInput "dec1pt1.txt")
        let instructions = map parseInstruction input
        let dial' = doInstructions instructions dial
        it "finds correct pt. 1 result" $
            finalZero dial' `shouldBe` 1048
        it "finds correct pt. 2 result" $
            runningZero dial' `shouldBe` 6498
