{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Taygeta.Tokenizer
    ( tokenC
    , numberFilter
    , tokenize
    , token
    , normalize
    , TokenType
    , Token(..)
    , TokenLoc(..)
    , TokenPos
    , FullToken
    , CA.PositionRange
    , CA.Position
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import           Data.Maybe (maybeToList)
import           Data.Monoid
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as P

type TokenType = T.Text

data Token = Token
    { tokenRaw  :: !T.Text
    , tokenText :: !TokenType
    } deriving (Eq, Show)

instance Monoid Token where
    mempty = Token mempty mempty
    mappend t1 t2 = Token (tokenRaw t1 `mappend` tokenRaw t2)
                          (tokenText t1 `mappend` tokenText t2)

-- This should probably use type families, similar to how persistent handles
-- IDs.
data TokenLoc = TokenLoc
    { tokenSource :: !TokenSource
    , tokenOffset :: !TokenOffset
    } deriving (Eq, Show)

type FullToken = (TokenLoc, Token)
type TokenPos  = (CA.PositionRange, Token)

type TokenSource = P.FilePath
type TokenOffset = Int

-- Conduits

tokenC :: (Monad m, C.MonadThrow m) => C.GLInfConduit T.Text m TokenPos
tokenC = CA.conduitParser token

numberFilter :: Monad m => C.Conduit TokenPos m TokenPos
numberFilter = loop []
    where
        loop seen = do
            current' <- C.await
            case current' of
                Nothing                     -> do
                    mapM_ C.yield  . maybeToList $ join seen
                    return ()
                Just current@(_, Token{..})
                    | tokenIsNumber tokenText -> loop (current:seen)
                    | isNumberSep tokenText && not (L.null seen) -> do
                        next <- CL.peek
                        if isNextNumber next
                            then loop (current:seen)
                            else do
                                mapM_ C.yield . maybeToList $ join seen
                                C.yield current
                                loop []
                    | otherwise -> do
                        mapM_ C.yield . maybeToList $ join seen
                        C.yield current
                        loop []

        tokenIsNumber = T.all isDigit

        isNumberSep "." = True
        isNumberSep "," = True
        isNumberSep _   = False

        isNextNumber (Just (_, Token{..})) | tokenIsNumber tokenText = True
        isNextNumber _                                               = False

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

