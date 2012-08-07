{-# LANGUAGE TypeFamilies #-}

module Taygeta.Corpus
    ( Corpus(..)
    , DocumentSource(..)
    , Document(..)
    , DocumentLocation(..)
    , textDocumentSource
    ) where


import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T


type ChunkSize  = Int
type Offset     = Int
type DocumentId = T.Text

class Corpus a where
    data DocumentSource a
    data Document a
    data DocumentLocation a

    getDocuments :: Monad m => C.Conduit (DocumentSource a) m (Document a)
    getDocumentChunks :: Monad m 
                      => ChunkSize
                      -> C.Conduit (Document a) m (DocumentLocation a, T.Text)

    moveOffset :: DocumentLocation a -> Int -> DocumentLocation a


instance Corpus T.Text where
    data DocumentSource T.Text = TextDocumentSource DocumentId T.Text
    data Document T.Text = TextDocument DocumentId T.Text
    data DocumentLocation T.Text = TextDocLocation DocumentId Int

    getDocuments = CL.map toTextDocument
    getDocumentChunks = CL.concatMap . chunkTextDocument

    moveOffset (TextDocLocation docId offset) delta =
        TextDocLocation docId $ offset + delta

textDocumentSource :: DocumentId -> T.Text -> DocumentSource T.Text
textDocumentSource docId text = TextDocumentSource docId text

toTextDocument :: DocumentSource T.Text -> Document T.Text
toTextDocument (TextDocumentSource docId text) =
    TextDocument docId text

chunkTextDocument :: ChunkSize
                  -> Document T.Text
                  -> [(DocumentLocation T.Text, T.Text)]
chunkTextDocument size (TextDocument docId text) =
    chunk text $ TextDocLocation docId 0
    where
        chunk text loc =
            let (a, b) = T.splitAt size text
            in  if T.null a
                    then []
                    else (loc, a) : (chunk b (loc `moveOffset` T.length a))

