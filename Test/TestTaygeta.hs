{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Monad.Identity
import           Control.Monad.Trans.Resource
import           Data.Char
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Monoid
import qualified Data.List as L
import qualified Data.Text as T
-- import qualified Filesystem.Path.CurrentOS as P
import           Text.Taygeta.Tokenizer
import           Test.Hspec
import           Test.QuickCheck

-- Specifications

main :: IO ()
main = hspec $ do
    describe "The tokenizer" $ do
        it "should return a list of tokens, the sum of whose lengths is the length of the input" $
            property pLengthSum

        it "should be able to recreate the input from the raw output" $
            property pIdem

        it "should tokenize alphabetic tokens" $ property $
            allCharsAre genAlphaList isAlpha

        it "should tokenize numeric tokens" $ property $
            allCharsAre genNumberList isDigit

        it "should tokenize punctuation" $
            allCharsAre genPunctuationList isPunctuation

        it "should tokenize punctuation one character each" $
            allTokensAre genPunctuationList ((1 ==) . T.length . tokenRaw)

        it "should return whitespace" $
            allCharsAre' genSpaceList isSpace

        it "should return marks" $
            allCharsAre genMarkList isMark

        it "should return symbols" $
            allCharsAre genSymbolList isSymbol

        it "should return separators" $
            allCharsAre genSeparatorList (\c -> isSeparator c && not (isSpace c))

    describe "The Token type" $ do
        it "should mappend mempty x == x" $ property $ \t ->
            let tkn = Token t $ normalize t
            in  mappend mempty tkn == tkn

        it "should mappend x mempty == x" $ property $ \t ->
            let tkn = Token t $ normalize t
            in  mappend tkn mempty == tkn

        it "should mappend x (mappend y z) == mappend (mappend x y) z" $ property $ \(tx, ty, tz) ->
            let tokenx = Token tx $ normalize tx
                tokeny = Token ty $ normalize ty
                tokenz = Token tz $ normalize tz
            in  mappend tokenx (mappend tokeny tokenz) == mappend (mappend tokenx tokeny) tokenz

    describe "The number filter" $ do
        it "should join numbers with common number punctuation" $ do
            let numbers = tokenC C.=$= numberFilter
                shouldParseTo input expected =
                    (input `shouldParseWith` numbers) expected
            "1"            `shouldParseTo` ["1"]
            "12"           `shouldParseTo` ["12"]
            "199.99"       `shouldParseTo` ["199.99"]
            "1,000"        `shouldParseTo` ["1,000"]
            "1,234,567.90" `shouldParseTo` ["1,234,567.90"]
            "1,22. "       `shouldParseTo` ["1,22", ".", " "]
            "1,,23.,45."   `shouldParseTo` ["1", ",", ",", "23", ".", ",", "45", "."]

    describe "The English token filter" $ do
        it "should remove whitespace" $
            pending "not implemented"

        it "join contractions" $
            pending "not implemented"

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink    = shrinkNothing

-- Utilities

edefault :: c -> (b -> c) -> Either a b -> c
edefault d f e = either (const d) f e

tokenize' :: T.Text -> [Token]
tokenize' input = runTokenConduit input tokenC

-- TODO: Somehow refactor this into Text.Taygeta.Tokenizer or *.Util or
-- *.Conduit or something.
runTokenConduit :: T.Text -> C.Conduit T.Text (ExceptionT Identity) TokenPos -> [Token]
runTokenConduit input conduit =
    edefault [] id . runIdentity $ runExceptionT conduit'
    where
        conduit' :: ExceptionT Identity [Token]
        conduit' =    CL.sourceList [input]
                 C.$= conduit
                 C.$= CL.map snd
                 C.$$ CL.consume

getChars :: (Char -> Bool) -> String
getChars = flip filter [minBound..maxBound]

shouldParseWith :: T.Text
                -> C.Conduit T.Text (ExceptionT Identity) TokenPos
                -> [T.Text] -> Expectation
shouldParseWith input filterC expected =
   (map tokenText (runTokenConduit input filterC)) `shouldBe` expected

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
genPunctuationList = genTextChars $ getChars isPunctuation

genText :: (Char -> Bool) -> Gen T.Text
genText = genTextChars . getChars

genPrintList :: Gen T.Text
genPrintList = genText isPrint

genSpaceList :: Gen T.Text
genSpaceList = genText isSpace

genMarkList :: Gen T.Text
genMarkList = genText isMark

genSymbolList :: Gen T.Text
genSymbolList = genText isSymbol

genSeparatorList :: Gen T.Text
genSeparatorList = genText (\c -> isSeparator c && not (isSpace c))

-- Predicates

allSame :: Eq a => [a] -> Bool
allSame []     = False
allSame (x:xs) = L.all (== x) xs

allCharsAre :: Gen T.Text -> (Char -> Bool) -> Property
allCharsAre gen p = forAll gen $ \n ->
    let tokens = filter ((" " /=) . tokenRaw) $ tokenize' n
    in  (length tokens > 0) .&&. (L.all (T.all p) $ map tokenRaw tokens)

allCharsAre' :: Gen T.Text -> (Char -> Bool) -> Property
allCharsAre' gen p = forAll gen $ \n ->
    let tokens = tokenize' n
    in  (length tokens > 0) .&&. (L.all (T.all p) $ map tokenRaw tokens)

allTokensAre :: Gen T.Text -> (Token -> Bool) -> Property
allTokensAre gen p = forAll gen $ \n ->
    let tokens = filter ((" " /=) . tokenRaw) $ tokenize' n
    in  (length tokens > 0) .&&. (L.all p tokens)

