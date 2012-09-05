{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Taygeta.Tokenizer
    ( tokenC
    , numberFilter
    , englishFilter
    , contractionFilter
    , whitespaceFilter
    , alphaNumericFilter
    , StopList
    , englishStopList
    , stopListFilter
    , englishStopListFilter
    , tokenize
    , token
    , normalize
    , TokenType
    , Token(..)
    , TokenLoc(..)
    , TokenPos
    , TextPos
    , FullToken
    , module Data.Conduit.Attoparsec
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.Conduit as C
import           Data.Conduit.Attoparsec (PositionRange(..), Position(..))
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.HashSet as S
import qualified Data.List as L
import           Data.Maybe (maybeToList)
import           Data.Monoid
import qualified Data.Text as T
import           Text.Taygeta.Types

-- Conduits

tokenC :: (Monad m, C.MonadThrow m) => C.GLInfConduit T.Text m TokenPos
tokenC = CA.conduitParser token

numberFilter :: Monad m => C.Conduit TokenPos m TokenPos
numberFilter = joinFilter (T.all isDigit . tokenText) (isNumberSep . tokenText)
    where
        isNumberSep "." = True
        isNumberSep "," = True
        isNumberSep _   = False

whitespaceFilter :: Monad m => C.Conduit TokenPos m TokenPos
whitespaceFilter = CL.filter (not . isWS)
    where
        isWS :: TokenPos -> Bool
        isWS (_, Token{..}) | T.all isSpace tokenText     = True
                            | T.all isSeparator tokenText = True
                            | otherwise                   = False

englishFilter :: Monad m => C.Conduit TokenPos m TokenPos
englishFilter = contractionFilter

contractionFilter :: Monad m => C.Conduit TokenPos m TokenPos
contractionFilter = joinFilter (T.all isAlpha . tokenText) ((== "'") . tokenText)

alphaNumericFilter :: Monad m => C.Conduit TokenPos m TokenPos
alphaNumericFilter = CL.filter (T.any isAlphaNum . tokenText . snd)

joinFilter :: Monad m
           => (Token -> Bool) -> (Token -> Bool)
           -> C.Conduit TokenPos m TokenPos
joinFilter isBodyToken isSepToken = loop []
    where
        loop seen = do
            current' <- C.await
            case current' of
                Nothing -> flush seen Nothing
                Just current@(_, t)
                    | isBodyToken t -> loop (current:seen)
                    | isSepToken t && not (L.null seen) -> do
                        next <- CL.peek
                        if isNextBody next
                            then loop (current:seen)
                            else flush seen (Just current) >> loop []
                    | otherwise -> flush seen (Just current) >> loop []

        flush seen current = do
            mapM_ C.yield . maybeToList $ join seen
            maybe (return ()) C.yield current

        isNextBody (Just (_, t)) = isBodyToken t
        isNextBody _             = False

        -- NB: It might be faster to pick the parts of the PositionRange apart
        -- and pull the start and end directly from the first and last
        -- elements, mconcat the tokens, and put the pair back together. But at
        -- the moment this seems more straightforward.
        join :: [TokenPos] -> Maybe TokenPos
        join []   = Nothing
        join [tp] = Just tp
        join tps  = Just $ L.foldl1' join' tps
            where
                join' :: TokenPos -> TokenPos -> TokenPos
                join' (CA.PositionRange _ prEnd, t2)
                      (CA.PositionRange prStart _, t1) =
                    (CA.PositionRange prStart prEnd, t1 <> t2)

-- Stop lists
englishStopList :: StopList
englishStopList = S.fromList
        [ "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you"
        , "your", "yours", "yourself", "yourselves", "he", "him", "his"
        , "himself", "she", "her", "hers", "herself", "it", "its", "itself"
        , "they", "them", "their", "theirs", "themselves", "what", "which"
        , "who", "whom", "this", "that", "these", "those", "am", "is", "are"
        , "was", "were", "be", "been", "being", "have", "has", "had"
        , "having", "do", "does", "did", "doing", "a", "an", "the", "and"
        , "but", "if", "or", "because", "as", "until", "while", "of", "at"
        , "by", "for", "with", "about", "against", "between", "into"
        , "through", "during", "before", "after", "above", "below", "to"
        , "from", "up", "down", "in", "out", "on", "off", "over", "under"
        , "again", "further", "then", "once", "here", "there", "when"
        , "where", "why", "how", "all", "any", "both", "each", "few", "more"
        , "most", "other", "some", "such", "no", "nor", "not", "only", "own"
        , "same", "so", "than", "too", "very", "s", "t", "can", "will", "just"
        , "don't", "should", "now"
        ]

stopListFilter :: Monad m => StopList -> C.Conduit TokenPos m TokenPos
stopListFilter stopList =
    CL.filter (not . flip S.member stopList . tokenText . snd)

englishStopListFilter :: Monad m => C.Conduit TokenPos m TokenPos
englishStopListFilter = stopListFilter englishStopList

-- Entry functions

tokenize :: TokenSource -> TokenOffset -> T.Text -> Either String [FullToken]
tokenize source offset = parseOnly (tokenList source offset)

-- Parser

tokenList :: TokenSource -> TokenOffset -> Parser [FullToken]
tokenList source offset =
    (snd . L.mapAccumL locate (TokenLoc source offset)) <$> many token
    where
        locate :: TokenLoc -> Token -> (TokenLoc, FullToken)
        locate loc@TokenLoc{..} t@Token{..} =
            ( loc { tokenOffset = tokenOffset + T.length tokenRaw }
            , (loc, t)
            )

token :: Parser Token
token = (
        spaceToken
    <|> alphaToken
    <|> digitToken
    <|> punctToken
    <|> markToken
    <|> symbolToken
    <|> separatorToken
    <|> anyToken
    )

token' :: (Char -> Bool) -> Parser Token
token' p = mkToken <$> takeWhile1 p

tchar :: (Char -> Bool) -> Parser Token
tchar p = mkToken . T.singleton <$> satisfy p

spaceToken :: Parser Token
spaceToken = token' isSpace

alphaToken :: Parser Token
alphaToken = token' isAlpha

digitToken :: Parser Token
digitToken = token' isDigit

punctToken :: Parser Token
punctToken = tchar isPunctuation

markToken :: Parser Token
markToken = token' isMark

symbolToken :: Parser Token
symbolToken = token' isSymbol

separatorToken :: Parser Token
separatorToken = token' isSeparator

anyToken :: Parser Token
anyToken = mkToken . T.singleton <$> anyChar

mkToken :: T.Text -> Token
mkToken t = Token t $ normalize t

-- Utilities

normalize :: T.Text -> T.Text
normalize = T.toLower

