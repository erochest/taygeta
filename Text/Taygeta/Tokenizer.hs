{-# LANGUAGE OverloadedStrings #-}

module Text.Taygeta.Tokenizer
    ( tokenize
    , TokenType
    , Token(..)
    , TokenLoc(..)
    , FullToken(..)
    ) where

import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as P

type TokenType = T.Text

data Token = Token
    { tokenRaw  :: T.Text
    , tokenText :: TokenType
    } deriving (Eq, Show)

-- This should probably use type families, similar to how persistent handles
-- IDs.
data TokenLoc = TokenLoc
    { tokenSource :: P.FilePath
    , tokenOffset :: Int
    } deriving (Eq, Show)

type FullToken = (TokenLoc, Token)

tokenize :: P.FilePath -> Int -> T.Text -> [FullToken]
tokenize source offset input = []

