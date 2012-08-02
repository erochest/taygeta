
module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "the test framework" $ do
        it "should fail sometimes" $
            (1 :: Int) `shouldBe` (2 :: Int)
        it "should pass sometimes" $
            (1 :: Int) `shouldBe` (1 :: Int)

