{-# LANGUAGE OverloadedStrings #-}

-- Some playing with things as I read through *Foundations of Statistical
-- Natural Language Processing* by Christopher Manning and Hinrich Shütze.

module Main where

import           Data.Conduit (($$), (=$))
import qualified Data.Conduit as C
import qualified Data.Conduit.Filesystem as CF
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Util as CU
import           Data.Hashable
import qualified Data.List as L
import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Filesystem.Path.CurrentOS as FS
import           System.Environment
import           Taygeta.Corpus
import           Text.Printf
import           Text.Taygeta.Tokenizer

-- Types

type Bigram        = (TokenType, TokenType)
type FreqMap a     = M.HashMap a Int
type InvertedIndex = M.HashMap TokenType [PositionRange]

-- Conduits and Sinks

bigrams :: Monad m => C.Conduit a m (a, a)
bigrams = C.await >>= maybe (return ()) loop
    where
        loop prev = do
            current' <- C.await
            case current' of
                Nothing      -> return ()
                Just current -> do
                    C.yield (prev, current)
                    loop current

countFreqsC :: (Monad m, Ord a, Hashable a) => C.Sink a m (FreqMap a)
countFreqsC = CL.fold inc M.empty
    where inc m t = M.insertWith (+) t 1 m

countBigrams :: Monad m => C.Sink TokenType m (FreqMap Bigram)
countBigrams = bigrams =$ countFreqsC

freqs :: Monad m => C.Sink TextPos m (FreqMap TokenType, FreqMap Bigram)
freqs = CL.map snd =$ CU.zipSinks countFreqsC countBigrams

indexTokens :: Monad m => C.Sink TextPos m InvertedIndex
indexTokens = CL.fold alter M.empty
    where alter m (l, t) = M.insertWith mappend t [l] m

count :: Monad m => C.Sink a m Int
count = CL.fold (\x _ -> x + 1) 0

-- Utilities

countFreqs :: (Ord a, Hashable a) => [a] -> FreqMap a
countFreqs = L.foldl' inc M.empty
    where inc m t = M.insertWith (+) t 1 m

sortFreqs :: FreqMap k -> [(k, Int)]
sortFreqs = L.reverse . L.sortBy (comparing snd) . M.toList

-- Reports

reportFreqs :: (Show a) => [(a, Int)] -> String
reportFreqs = L.foldr format [] . zip [1..]
    where
        format (r, (a, f)) s =
            (printf "%4d. %20s %-5d %d\n" r (show a) f (r * f)) ++ s

kwic :: [T.Text] -> Int -> InvertedIndex -> T.Text -> [T.Text]
kwic textLines width index target =
    map getContext
        . catMaybes
        . map findIndex
        . L.sort
        $ M.lookupDefault [] target index
    where
        csize :: Int
        csize = round $ (fromIntegral width / 2 :: Double)
                      - (fromIntegral $ T.length target) / (2 :: Double)

        rmnl '\n' = ' '
        rmnl '\r' = ' '
        rmnl c    = c

        text = T.map rmnl $ T.concat textLines

        lineLengths = M.fromList . zip [1..] . snd
                    $ L.mapAccumL accumLengths 0 textLines
        accumLengths sofar line = (len, sofar)
            where len = sofar + T.length line

        findIndex :: PositionRange -> Maybe Int
        findIndex (PositionRange (Position line col) _) =
            (+ col) `fmap` M.lookup line lineLengths

        -- Umm. There has to be a better way.
        getContext l =
            let start    = max 0 $ l - csize
                c        = T.take width $ T.drop start text
                (p, c')  = T.breakOn " " c
                (c'', _) = T.breakOnEnd " " c'
                context  = (T.pack . take (T.length p) $ repeat ' ') `mappend` c''
            in  if start > 0
                    then context
                    else (T.pack . take (abs $ l - csize) $ repeat ' ') `mappend` context


nl :: IO ()
nl = putStrLn ""

header :: String -> IO ()
header filename = do
    putStrLn filename
    putStrLn . take (length filename) $ repeat '='

tokenFreqReport :: Int -> FreqMap TokenType -> IO ()
tokenFreqReport limit = putStrLn . reportFreqs . take limit . sortFreqs

freqReport :: Int -> FreqMap TokenType -> IO ()
freqReport tokenCount typeFreqs =  printf "Total tokens = %d\n" tokenCount
                                >> printf "Total types  = %d\n" (M.size typeFreqs)

freqFreqReport :: FreqMap Int -> IO ()
freqFreqReport = 
    putStrLn . reportFreqs . L.sortBy (comparing fst) . M.toList

bigramReport :: Int -> FreqMap Bigram -> IO ()
bigramReport limit = putStrLn . reportFreqs . take limit . sortFreqs

main :: IO ()
main = do
    args <- getArgs
    (filename, mtarget) <- case args of
        (f:t:_) -> return (f, Just t)
        (f:_)   -> return (f, Nothing)
        []      -> fail "You must provide a filename to process."


    (((tfreqs, bfreqs), index), tokenCount) <- C.runResourceT $
           CF.sourceFile (FS.decodeString filename)
        $$ getDocumentText
        =$ tokenC
        =$ englishFilter
        =$ whitespaceFilter
        =$ CL.map (fmap tokenText)
        =$ CU.zipSinks (CU.zipSinks freqs indexTokens) count

    text <- T.lines `fmap` TIO.readFile filename
    let ffreqs  = countFreqs $ M.elems tfreqs

    header filename              >> nl
    tokenFreqReport 20 tfreqs    >> nl
    freqReport tokenCount tfreqs >> nl
    freqFreqReport ffreqs        >> nl
    bigramReport 20 bfreqs       >> nl

    mapM_ (putStrLn . T.unpack) $ maybe [] (kwic text 78 index . T.pack) mtarget


