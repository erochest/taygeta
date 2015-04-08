{-# LANGUAGE OverloadedStrings #-}


module Taygeta.TokenizerSpec where


import Test.Hspec

import Taygeta.Tokenizer


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

