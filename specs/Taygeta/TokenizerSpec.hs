{-# LANGUAGE OverloadedStrings #-}


module Taygeta.TokenizerSpec where


import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as A
import           Data.Char            (isSpace)
import qualified Data.Text            as T

import           Test.Hspec

import           Taygeta.Tokenizer


spec :: Spec
spec = do
    describe "splitTokenizer" $ do
        it "should split a string on whitespace." $
            splitTokenizer "the red pony rides the moors" `shouldBe`
                ["the", "red", "pony", "rides", "the", "moors"]
        it "should not separate puncutation from adjoining words." $
            splitTokenizer "the red pony! it rides the moors!" `shouldBe`
                ["the", "red", "pony!", "it", "rides", "the", "moors!"]
        it "should not normalize case (or anything else)." $
            splitTokenizer "The red pony! It rides the Moors!" `shouldBe`
                ["The", "red", "pony!", "It", "rides", "the", "Moors!"]

    describe "parserTokenizer" $ do
        it "should return all matches of a given parser." $ do
            let space' = T.singleton <$> space
                rWords = T.cons <$> (space *> char 'r') <*> A.takeWhile (not . isSpace)
            parserTokenizer (string "the") "the red pony rides the moors" `shouldBe`
                ["the", "the"]
            parserTokenizer space' "the red pony rides the moors" `shouldBe`
                [" ", " ", " ", " ", " "]
            parserTokenizer space' " the red pony rides the moors " `shouldBe`
                [" ", " ", " ", " ", " ", " ", " "]
            parserTokenizer rWords "the red pony rides the moors" `shouldBe`
                ["red", "rides"]
            parserTokenizer rWords "red pony rides the moors" `shouldBe`
                ["rides"]

    describe "sexprTokenizer" $ do
        it "should break inputs on whitespace." $
            sexprTokenizer "a b c d e" `shouldBe` ["a", "b", "c", "d", "e"]
        it "should return parenthesized items as a group." $
            sexprTokenizer "a (b c d) e" `shouldBe` ["a", "(b c d)", "e"]
        it "should handle nested parentheses." $
            sexprTokenizer "(a (b (c) d)) e" `shouldBe` ["(a (b (c) d))", "e"]
        it "should return nothing if the parentheses aren't balanced." $ do
            sexprTokenizer "(a (b (c d)) e" `shouldBe` []
            sexprTokenizer "a (b (c d)) e)" `shouldBe` []
        it "should handle the NLTK docstring examples." $
            sexprTokenizer "(a b (c d)) e f (g)" `shouldBe` ["(a b (c d))", "e", "f", "(g)"]

    describe "charTokenizer" $
        it "should break a string into characters." $
            charTokenizer "abcdefgh ijklm" `shouldBe` "abcdefgh ijklm"

    describe "lineTokenizer" $
        it "should break a string into lines." $
            lineTokenizer "abc\ndef\nhijk lm\n" `shouldBe`
                ["abc", "def", "hijk lm"]

    describe "treebankTokenizer" $ do
        it "should split standard contractions." $ do
            treebankTokenizer "don't" `shouldBe` ["do", "n't"]
            treebankTokenizer "they'll" `shouldBe` ["they", "'ll"]
        it "should treat most punctuation characters as separate tokens." $
            treebankTokenizer "hello! how are you?" `shouldBe`
                ["hello", "!", "how", "are", "you", "?"]
        it "should split off commas and single quotes when followed by whitespace." $
            treebankTokenizer "'hello,' jackie." `shouldBe`
                ["'hello", ",", "'", "jackie", "."]
        it "should separate periods that appear at the end of a line." $
            treebankTokenizer "this is the doctor." `shouldBe`
                ["this", "is", "the", "doctor", "."]
        it "should not separate periods within a line." $
            treebankTokenizer "the dr. is in." `shouldBe`
                ["the", "dr.", "is", "in", "."]
        it "should tokenize some examples." $ do
            treebankTokenizer "Good muffins cost $3.88\n\
                              \in New York.  Please buy me\n\
                              \two of them.\nThanks.\n" `shouldBe`
                ["Good", "muffins", "cost", "$", "3.88", "in", "New", "York.",
                 "Please", "buy", "me", "two", "of", "them.", "Thanks", "."]
            treebankTokenizer "They'll save and invest more." `shouldBe`
                ["They", "'ll", "save", "and", "invest", "more", "."]

    describe "regexTokenizer" $ do
        it "should identify everything that matches a regex." $
            let tokenizer = regexTokenizer "\\p{L}[\\p{L}\\p{P}]*\\p{L}"
            in  tokenizer "Good muffins cost $3.88\n\
                          \in New York.  Please buy me\n\
                          \two of them.\nThanks.\n" `shouldBe`
                ["Good", "muffins", "cost", "in", "New", "York",
                 "Please", "buy", "me", "two", "of", "them", "Thanks"]
