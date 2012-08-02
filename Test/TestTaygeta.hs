{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Data.Char
import qualified Data.List as L
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as P
import           Text.Taygeta.Tokenizer
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink    = shrinkNothing

-- Utilities

edefault :: c -> (b -> c) -> Either a b  -> c
edefault d f e = either (const d) f e

tokenize' :: T.Text -> [Token]
tokenize' = edefault [] (map snd) . tokenize P.empty 0

-- Properties

pLengthSum :: T.Text -> Bool
pLengthSum input =
    T.length input == (sum $ map (T.length . tokenRaw) (tokenize' input))

pIdem :: T.Text -> Bool
pIdem input =
    input == (T.concat $ map tokenRaw (tokenize' input))

-- Generators

genList :: String -> Gen T.Text
genList = fmap (T.intercalate " ") . listOf1 . genTextChars

genTextChars :: String -> Gen T.Text
genTextChars set =
    fmap T.pack . listOf1 $ elements set

genAlphaList :: Gen T.Text
genAlphaList = genList $ ['a'..'z'] ++ ['A'..'Z']

genNumberList :: Gen T.Text
genNumberList = genList $ ['0'..'9']

genPunctuationList :: Gen T.Text
genPunctuationList = genTextChars $ filter isPunctuation [minBound..maxBound]

genPrintList :: Gen T.Text
genPrintList = genTextChars $ filter isPrint [minBound..maxBound]

genSpaceList :: Gen T.Text
genSpaceList = genTextChars $ filter isSpace [minBound..maxBound]

-- Predicates

allSame :: Eq a => [a] -> Bool
allSame []     = False
allSame (x:xs) = L.all (== x) xs

allTokensAre :: Gen T.Text -> (Char -> Bool) -> Property
allTokensAre gen p = forAll gen $ \n ->
    let tokens = filter ((" " /=) . tokenRaw) $ tokenize' n
    in  (length tokens > 0) .&&. (L.all (T.all p) $ map tokenRaw tokens)

-- Specifications

main :: IO ()
main = hspec $ do
    describe "The tokenizer" $ do
        it "should return a list of tokens, the sum of whose lengths is the length of the input" $
            property pLengthSum

        it "should be able to recreate the input from the raw output" $
            property pIdem

        it "should tokenize alphabetic tokens" $ property $
            allTokensAre genAlphaList isAlpha

        it "should tokenize numeric tokens" $ property $
            allTokensAre genNumberList isDigit

        it "should tokenize punctuation" $
            allTokensAre genPunctuationList isPunctuation

        it "should return whitespace" $
            allTokensAre genSpaceList isSpace

    describe "The number filter" $ do
        it "should join numbers with common number punctuation" $
            pending "not implemented"

    describe "The English token filter" $ do
        it "should remove whitespace" $
            pending "not implemented"

        it "join contractions" $
            pending "not implemented"

