
-- | This contains all the types used in 'Taygeta'.
module Text.Taygeta.Types
    ( -- * Tokens
      TokenType
    , Token(..)
      -- ** Token Locations
    , TokenSource
    , TokenOffset
    , TokenLoc(..)
      -- ** Tokens and Locations
    , TokenPos
    , TextPos
    , FullToken
      -- ** Other Types
    , StopList
      -- * Other Types
    , FreqMap
    ) where

import           Data.Conduit.Attoparsec (PositionRange)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import           Data.Monoid
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as P

-- | This represents a map of items to their frequencies.
type FreqMap a     = M.HashMap a Int

-- | This is the type of raw (and processed) token data.
type TokenType = T.Text

-- | This represents a token.
data Token = Token
    { -- | The raw token text, unchanged from the input.
      tokenRaw  :: !T.Text
      -- | The normalized token text.
    , tokenText :: !TokenType
    } deriving (Eq, Show)

instance Monoid Token where
    mempty = Token mempty mempty
    mappend t1 t2 = Token (tokenRaw t1 `mappend` tokenRaw t2)
                          (tokenText t1 `mappend` tokenText t2)

-- | This represents where a token came from, both its input source and its
-- offset in the source.
--
-- This should probably use type families, similar to how persistent handles
-- IDs.
data TokenLoc = TokenLoc
    { -- | What was the source of the token?
      tokenSource :: !TokenSource
      -- | Where does the token occur in the input source?
    , tokenOffset :: !TokenOffset
    } deriving (Eq, Show)

-- | This is a full token's information, both the 'TokenLoc' and the 'Token'
-- itself.
type FullToken = (TokenLoc, Token)

-- | This is the position of the 'Token' in the input. The 'PositionRange'
-- information is taken from 'Data.Conduit.Attoparsec.Parser'.
type TokenPos  = (PositionRange, Token)

-- | This is the position of a bare 'TokenType' in the input. The
-- 'PositionRange' information is taken from 'Data.Conduit.Attoparsec.Parser'.
type TextPos   = (PositionRange, TokenType)

-- | This is the type for tokens' sources.
type TokenSource = P.FilePath
-- | This is the type for tokens' offsets in the original input.
type TokenOffset = Int

-- | This is a type alias for stop lists (and other sets of tokens).
type StopList = S.HashSet T.Text


