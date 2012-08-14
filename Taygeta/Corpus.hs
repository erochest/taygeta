{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO add a parameter on Corpus for the type of data in the files.

module Taygeta.Corpus
    ( Document(..)
    , chunkOn
    ) where


import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.State.Class (MonadState)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import           Data.Conduit.Filesystem
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import           Data.Lens.Common
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Filesystem.Path.CurrentOS
import           Prelude hiding (FilePath)


-- Sketching out things:
-- =====================
--
-- Corpus is no longer needed, because we'll let Data.Conduit.Attoparsec take
-- care of the position in the input stream. Moreover, if we only handle single
-- documents (or single token streams, at least), we can use closures to track
-- the current document and current document position.
--
-- Document is still needed, though, because it handles converting format x
-- into a Text stream.
--

-- Basic type and class definitions.

class Document a where
    getDocumentText :: (Monad m, C.MonadThrow m) => C.Conduit a m T.Text

instance Document B.ByteString where
    getDocumentText = CT.decode CT.utf8

instance Document T.Text where
    getDocumentText = CL.map id

-- This chunks on a text, returning the prefix each time, and the next time
-- concatenating the rest with the prefix from the next input.
chunkOn :: Monad m => T.Text -> C.Conduit T.Text m T.Text
chunkOn t = loop T.empty
    where
        loop prefix = do
            text' <- C.await
            case text' of
                Nothing -> handleTail prefix
                Just text -> do
                    let (current, next) = T.breakOnEnd t text
                    C.yield current
                    loop next

        handleTail prefix | T.null prefix = return ()
                          | otherwise     = C.leftover prefix >> return ()


