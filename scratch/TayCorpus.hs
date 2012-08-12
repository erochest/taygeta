{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Control ()
import           Control.Monad.Trans.Resource ()
import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString as B
import           Data.Conduit (($=), ($$), (=$), await, yield, runPipe)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Internal as CI
import           Data.Conduit.List (sourceList, consume, concatMap)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Filesystem.Path.CurrentOS
import           Prelude hiding (concatMap, FilePath)
import           Taygeta.Corpus
import           System.IO (stdout)

-- Processing

process :: (Corpus m a, Monad m, MonadIO m, C.MonadThrow m)
        => a -> CorpusT m ()
process a =  sourceList [a]
          $= getDocuments
          $= (getDocumentData :: C.MonadThrow n => C.Conduit B.ByteString (CorpusT n) T.Text)
          $$ getDocumentText
          =$ showLoc
          =$ CT.encode CT.utf8
          =$ CB.sinkHandle stdout

-- Utility methods

showLoc :: Monad m
        => C.Conduit T.Text (CorpusT m) T.Text
showLoc = do
    text' <- await
    case text' of
        Nothing -> return ()
        Just text -> do
            DocumentLocation{..} <- lift get
            yield ">>> "
            yieldNothing documentId
            yield "#"
            yieldNothing documentAnchor
            yield ":"
            yieldShow $ getSum documentOffset
            yield " => '"
            yield text
            yield "'\n"
            lift . moveOffset $ T.length text
            showLoc

yieldShow :: (Show a, Monad m) => a -> C.Pipe l i T.Text u m ()
yieldShow = yield . T.pack . show

yieldNothing :: (Show a, Monad m) => Maybe a -> C.Pipe l i T.Text u m ()
yieldNothing = yield . maybe "" (T.pack . show)

-- main

main :: IO ()
-- main = evalCorpus $ process ("Good-bye, cruel world." :: B.ByteString)
main = C.runResourceT $ evalCorpus $ process ("LICENSE" :: FilePath)

