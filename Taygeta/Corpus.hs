{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO add a parameter on Corpus for the type of data in the files.

module Taygeta.Corpus
    ( DocumentId
    , DocumentAnchor
    , Offset
    , CorpusT(..)
    , Corpus(..)
    , Document(..)
    , DocumentLocation(..)
    , evalCorpus
    , offsetInt
    , setOffset'
    , moveOffset'
    , setOffset
    , moveOffset
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
-- Corpus source format       -> C.Source m (Document format)
-- Document format            -> C.Source m format
-- DocumentLocation format
-- 
-- Corpus Text Text           -> C.Source m (Document Text)
-- Corpus FilePath Text       -> C.Source m (Document Text)
-- Corpus URL Text            -> C.Source m (Document Text)
-- 
-- Corpus Text X.Document     -> C.Source m (Document X.Document)
-- Corpus FilePath X.Document -> C.Source m (Document X.Document)
-- Corpus URL X.Document      -> C.Source m (Document X.Document)
-- 
-- Document Text              -> C.Source m Text
-- Document X.Document        -> C.Source m Text
-- 
-- * What method gets from the Document f to the ByteString/Text, forall f?
-- * How do we get the DocumentLocation downstream from (Document f) ->
--   (C.Source m Text)? Seems like we need to wrap the location in a tuple
--   that also embeds another conduit. Something like (DocumentLocation
--   format, C.Source m Text).
-- 

-- Basic type and class definitions.

type DocumentId     = T.Text
type DocumentAnchor = T.Text
type Offset         = Sum Int


type CorpusT m = StateT DocumentLocation m

evalCorpus :: Monad m => CorpusT m a -> m a
evalCorpus m = evalStateT m state
    where state = DocumentLocation Nothing Nothing mempty

class Corpus m a where
    getDocuments :: C.Conduit a m B.ByteString

class Document a where
    getDocumentData :: (Monad m, C.MonadThrow m) => C.Conduit B.ByteString m a
    getDocumentText :: (Monad m, C.MonadThrow m) => C.Conduit a m T.Text

data DocumentLocation = DocumentLocation
    { documentId     :: Maybe DocumentId
    , documentAnchor :: Maybe DocumentAnchor
    , documentOffset :: Offset
    } deriving (Show, Eq)

offsetInt :: Lens DocumentLocation Int
offsetInt = lens (getSum . documentOffset) $ \offset dl ->
    dl { documentOffset = Sum offset }

setOffset' :: Int -> DocumentLocation -> DocumentLocation
setOffset' = setL offsetInt

moveOffset' :: Int -> DocumentLocation -> DocumentLocation
moveOffset' delta = modL offsetInt (+ delta)

setOffset :: Monad m => Int -> CorpusT m ()
setOffset offset = modify (setOffset' offset)

moveOffset :: Monad m => Int -> CorpusT m ()
moveOffset delta = modify (moveOffset' delta)

-- Corpus Text
--
-- This corpus is made up of one document with a byte string. (Text strings can
-- be created by creating a Document Text directly.)
instance Monad m => Corpus (CorpusT m) B.ByteString where
    getDocuments = CL.mapM putLocation
        where putLocation i = do
                    put $ DocumentLocation (Just "<byte-string>")
                                                  Nothing mempty
                    return i

instance Monad m => Corpus m B.ByteString where
    getDocuments = CL.map id

instance Document T.Text where
    getDocumentData = CT.decode CT.utf8
    getDocumentText = CL.map id

-- Corpus FilePath
-- This corpus is all the files in a directory.
instance (Applicative m, MonadIO m, C.MonadThrow m, C.MonadUnsafeIO m)
      => Corpus (CorpusT (C.ResourceT m)) FilePath
      where
    getDocuments = do
        fp' <- C.await
        case fp' of
            Nothing -> return ()
            Just fp -> do
                lift . put $ DocumentLocation (Just $ safePath fp) Nothing mempty
                sourceFile fp
                getDocuments
        where safePath = safePath' . toText
              safePath' (Left t)  = t
              safePath' (Right t) = t

instance (Applicative m, MonadIO m, C.MonadThrow m, C.MonadUnsafeIO m)
      => Corpus (C.ResourceT m) FilePath
      where
    getDocuments = do
        fp' <- C.await
        case fp' of
            Nothing -> return ()
            Just fp -> do
                sourceFile fp
                getDocuments

