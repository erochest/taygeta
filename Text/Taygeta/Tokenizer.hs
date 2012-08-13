{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Taygeta.Tokenizer
    ( tokenize
    , token
    , normalize
    , TokenType
    , Token(..)
    , TokenLoc(..)
    , FullToken
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.List as L
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as P

type TokenType = T.Text

data Token = Token
    { tokenRaw  :: !T.Text
    , tokenText :: !TokenType
    } deriving (Eq, Show)

-- This should probably use type families, similar to how persistent handles
-- IDs.
data TokenLoc = TokenLoc
    { tokenSource :: !TokenSource
    , tokenOffset :: !TokenOffset
    } deriving (Eq, Show)

type FullToken = (TokenLoc, Token)

type TokenSource = P.FilePath
type TokenOffset = Int

tokenize :: TokenSource -> TokenOffset -> T.Text -> Either String [FullToken]
tokenize source offset = parseOnly (tokenList source offset)

normalize :: T.Text -> T.Text
normalize = id

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

