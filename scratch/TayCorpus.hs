{-# LANGUAGE FlexibleContexts #-}
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
        => a -> m ()
process a =  sourceList [a]
          $= getDocuments
          $= (getDocumentData :: C.MonadThrow n => C.Conduit B.ByteString n T.Text)
          $$ getDocumentText
          =$ showLoc
          =$ CT.encode CT.utf8
          =$ CB.sinkHandle stdout

-- Utility methods

showLoc :: Monad m => C.Conduit T.Text m T.Text
showLoc = do
    text' <- await
    case text' of
        Nothing -> return ()
        Just text -> do
            yield ">>> '"
            yield text
            yield "'\n"
            showLoc

-- main

main :: IO ()
main = process ("Good-bye, cruel world." :: B.ByteString)
-- main = C.runResourceT $ process ("LICENSE" :: FilePath)

