
module Text.Taygeta.Types
    ( FreqMap
    , TokenType
    , Token(..)
    , TokenLoc(..)
    , FullToken
    , TokenPos
    , TextPos
    , TokenSource
    , TokenOffset
    , StopList
    ) where

import           Data.Conduit.Attoparsec (PositionRange)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import           Data.Monoid
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as P

type FreqMap a     = M.HashMap a Int

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
type TokenPos  = (PositionRange, Token)
type TextPos   = (PositionRange, TokenType)

type TokenSource = P.FilePath
type TokenOffset = Int

type StopList = S.HashSet T.Text


