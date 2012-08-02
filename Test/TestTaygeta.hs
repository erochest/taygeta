
module Main where

import Text.Taygeta.Tokenizer
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "The tokenizer" $ do
        it "should return a list of tokens" $
            pending "not implemented"

        it "should return a list of tokens, the sum of whose lengths is the length of the input" $
            pending "not implemented"

        it "should be able to recreate the input from the raw output" $
            pending "not implemented"

        it "should tokenize alphabetic tokens" $
            pending "not implemented"

        it "should tokenize numeric tokens" $
            pending "not implemented"

        it "should tokenize punctuation" $
            pending "not implemented"

        it "should tokenize random characters" $
            pending "not implemented"

        it "should return whitespace" $
            pending "not implemented"

    describe "The number filter" $ do
        it "should join numbers with common number punctuation" $
            pending "not implemented"

    describe "The English token filter" $ do
        it "should remove whitespace" $
            pending "not implemented"

        it "join contractions" $
            pending "not implemented"

