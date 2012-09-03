

module Taygeta.Corpus
    ( chunkOn
    ) where


import           Control.Monad
import qualified Data.Conduit as C
import qualified Data.Text as T


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
                          | otherwise     = void (C.leftover prefix)

