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
