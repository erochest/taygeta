{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Control ()
import           Control.Monad.Trans.Resource ()
import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString as B
import           Data.Conduit (($=), ($$), await, yield, runPipe)
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

readByteString :: IO ()
readByteString = 
    evalCorpus $  sourceList ["Good-bye, cruel world." :: B.ByteString]
               $= getDocuments
               $= (getDocumentData :: C.Conduit B.ByteString (CorpusT IO) T.Text)
               $= getDocumentText
               $= showLoc
               $= CT.encode CT.utf8
               $$ CB.sinkHandle stdout

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

readFilePath :: IO ()
readFilePath = C.runResourceT $
    evalCorpus $  sourceList ["texts/pg74.txt" :: FilePath]
               $= getDocuments
               $= (getDocumentData :: C.Conduit B.ByteString (CorpusT (C.ResourceT IO)) T.Text)
               $= getDocumentText
               $= showLoc
               $= CT.encode CT.utf8
               $$ CB.sinkHandle stdout

main :: IO ()
main = readFilePath

