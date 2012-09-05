{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This has the base tokenizer, both as a parser and 'C.Conduit', and
-- a series of filters that can be composed on top of the bases.
--
-- The primary interface is the 'C.Conduit' interface, and most of the filters
-- are for that.
module Text.Taygeta.Tokenizer
    ( -- * Data Types
      -- ** Base Types
      TokenType
    , Token(..)
      -- ** Types and Locations
    , FullToken
    , TokenPos
    , TextPos
    , TokenLoc(..)
      -- * Conduits
    , tokenC
      -- ** Conduit Filters
    , numberFilter
    , englishFilter
    , contractionFilter
    , whitespaceFilter
    , alphaNumericFilter
      -- ** Stop List Filters
    , StopList
    , englishStopList
    , stopListFilter
    , englishStopListFilter
      -- * Parsers and Functions
    , tokenize
    , normalize
    , token
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

-- | This breaks input 'T.Text' up into a stream of 'TokenPos'.
tokenC :: (Monad m, C.MonadThrow m) => C.GLInfConduit T.Text m TokenPos
tokenC = CA.conduitParser token

-- | This groups tokens that comprise numbers with commas and periods into
-- a single 'Token'.
numberFilter :: Monad m => C.Conduit TokenPos m TokenPos
numberFilter = joinFilter (T.all isDigit . tokenText) (isNumberSep . tokenText)
    where
        isNumberSep "." = True
        isNumberSep "," = True
        isNumberSep _   = False

-- | This removes whitespace tokens from the input stream.
whitespaceFilter :: Monad m => C.Conduit TokenPos m TokenPos
whitespaceFilter = CL.filter (not . isWS)
    where
        isWS :: TokenPos -> Bool
        isWS (_, Token{..}) | T.all isSpace tokenText     = True
                            | T.all isSeparator tokenText = True
                            | otherwise                   = False

-- | This groups tokens into English-y words.
--
-- Currently, this just groups /alpha-numeric/, /single quote/, and
-- /alpha-numeric/ tokens into a single token.
englishFilter :: Monad m => C.Conduit TokenPos m TokenPos
englishFilter = contractionFilter

-- | This groups /alpha-numeric/, /single quote/, and /alpha-numeric/ tokens
-- into a single 'Token'.
contractionFilter :: Monad m => C.Conduit TokenPos m TokenPos
contractionFilter = joinFilter (T.all isAlpha . tokenText) ((== "'") . tokenText)

-- | This filters out non-alphanumeric tokens.
alphaNumericFilter :: Monad m => C.Conduit TokenPos m TokenPos
alphaNumericFilter = CL.filter (T.any isAlphaNum . tokenText . snd)

-- | This groups tokens according to the output of two predicates.
joinFilter :: Monad m
           => (Token -> Bool)
           -- ^ The 'isBodyToken' predicate tests whether the 'Token' is a body
           -- 'Token'. One or more body 'Token's are joined.
           -> (Token -> Bool)
           -- ^ The 'isSepToken' predicate tests whether the 'Token' is
           -- a separator. Only a single separator is accepted at one time.
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

-- | This is a set of English stop words. This is taken from the data for NLTK
-- (<http://nltk.org/>).
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

-- | This takes a 'StopList' and constructs a 'C.Conduit' filtering out
-- 'Token's from the list.
stopListFilter :: Monad m => StopList -> C.Conduit TokenPos m TokenPos
stopListFilter stopList =
    CL.filter (not . flip S.member stopList . tokenText . snd)

-- | This is a pre-composed English stop list filter.
englishStopListFilter :: Monad m => C.Conduit TokenPos m TokenPos
englishStopListFilter = stopListFilter englishStopList

-- Entry functions

-- | This tokenizes an input 'T.Text' and either returns an error message
-- 'String' or a list of 'FullToken'.
tokenize :: TokenSource
         -- ^ The source for the 'T.Text'.
         -> TokenOffset
         -- ^ The initial offset of the input in the source.
         -> T.Text
         -- ^ The input.
         -> Either String [FullToken]
         -- ^ An error 'String' or the list of tokens.
tokenize source offset = parseOnly (tokenList source offset)

-- Parser

-- ^ A parser over a list of 'Token's.
tokenList :: TokenSource
          -- ^ The source for the input.
          -> TokenOffset
          -- ^ The offset of the input in the source.
          -> Parser [FullToken]
tokenList source offset =
    (snd . L.mapAccumL locate (TokenLoc source offset)) <$> many token
    where
        locate :: TokenLoc -> Token -> (TokenLoc, FullToken)
        locate loc@TokenLoc{..} t@Token{..} =
            ( loc { tokenOffset = tokenOffset + T.length tokenRaw }
            , (loc, t)
            )

-- | This parsers a single 'Token'.
--
-- This breaks up the input without throwing anything away.
token :: Parser Token
token =   spaceToken
      <|> alphaToken
      <|> digitToken
      <|> punctToken
      <|> markToken
      <|> symbolToken
      <|> separatorToken
      <|> anyToken

-- | This constructs a parser from a predicate. The 'Parser' accepts a span of
-- the input and produces a 'Token'.
token' :: (Char -> Bool) -> Parser Token
token' p = mkToken <$> takeWhile1 p

-- | This constructs a parser from a predicate. The 'Parser' accepts a single characters and produces a 'Token'.
tchar :: (Char -> Bool) -> Parser Token
tchar p = mkToken . T.singleton <$> satisfy p

-- | This parses a string of white space.
spaceToken :: Parser Token
spaceToken = token' isSpace

-- | This parses a string of alphabetic characters.
alphaToken :: Parser Token
alphaToken = token' isAlpha

-- | This parses a string of numeric characters.
digitToken :: Parser Token
digitToken = token' isDigit

-- | This parses a single punctuation character.
punctToken :: Parser Token
punctToken = tchar isPunctuation

-- | This parses a string of marks.
markToken :: Parser Token
markToken = token' isMark

-- | This parses a string of symbols.
symbolToken :: Parser Token
symbolToken = token' isSymbol

-- | This parses a string of separator characters.
separatorToken :: Parser Token
separatorToken = token' isSeparator

-- | This parses a single character of any type.
anyToken :: Parser Token
anyToken = mkToken . T.singleton <$> anyChar

-- | This takes some text and wraps it in a 'Token'.
mkToken :: T.Text -> Token
mkToken t = Token t $ normalize t

-- Utilities

-- | This normalizes the raw token text.
normalize :: T.Text -> T.Text
normalize = T.toLower

