
-- | This contains utilities and data types for working with corpus- and
-- document-level processing.
module Taygeta.Corpus
    ( chunkOn
    ) where

import           Control.Monad
import qualified Data.Conduit as C
import qualified Data.Text as T

-- | This chunks on a text, returning the prefix each time, and the next time
-- concatenating the rest with the prefix from the next input.
--
-- This re-chunks the input. For each chunk received, it splits from the right
-- on the text 't', and the tail is prepended to the next chunk or output by
-- itself if there are no chunks.
chunkOn :: Monad m
        => T.Text
        -- ^ Text to split the chunks on.
        -> C.Conduit T.Text m T.Text
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
                          | otherwise     = void (C.leftover prefix)

