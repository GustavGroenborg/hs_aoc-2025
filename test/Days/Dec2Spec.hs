module Days.Dec2Spec where

import Days.Dec2
import Test.Hspec
import Util

-- Expected example result: 1227775554

spec :: Spec
spec = do
    describe "splitBy" $ do
        it "fnds a singleton range" $
            splitBy "abc" ',' `shouldBe` ("abc", "")
        it "finds a range between multiple" $
            splitBy "abc,def" ',' `shouldBe` ("abc", "def")
    describe "findRanges"  $ do
        it "returns empty list on base case" $
            findRanges "" `shouldBe` []
        it "finds all possible ranges" $
            findRanges "abc,def,ghi" `shouldBe` ["abc", "def", "ghi"]
    describe "checkInvalidNumber" $ do
        describe "invalid numbers" $ do
            it "should return 11" $ do
                checkInvalidNumber 11 `shouldBe` Just 11
            it "should return 123123" $
                checkInvalidNumber 123123 `shouldBe` Just 123123
        describe "valid numbers" $ do
            it "should not return 1" $
                checkInvalidNumber 1 `shouldBe` Nothing
            it "should not return 95" $
                checkInvalidNumber 95 `shouldBe` Nothing
            it "should not return 111" $
                checkInvalidNumber 111 `shouldBe` Nothing
    describe "exampleInput" $ do
        input <- runIO (getExampleInput "dec2pt1.txt")
        let ranges = findRanges (head input)
        it "finds correct pt.1 result" $
            (sum $ map findInvalidSum ranges) `shouldBe` 1227775554
    describe "puzzleInput" $ do
        input <- runIO (getPuzzleInput "dec2pt1.txt")
        let ranges = findRanges (head input)
        it "finds correct pt.2 result" $
            (sum $ map findInvalidSum ranges) `shouldBe` 0
