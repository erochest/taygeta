{-# LANGUAGE OverloadedStrings #-}

-- | Some playing with things as I read through /Foundations of Statistical
-- Natural Language Processing/ by Christopher Manning and Hinrich Shütze.
module Main where

import           Data.Conduit (($$), (=$))
import qualified Data.Conduit as C
import qualified Data.Conduit.Filesystem as CF
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Util as CU
import           Data.Hashable
import qualified Data.List as L
import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord (comparing)
import           Data.Taygeta.Freqs hiding (countFreqs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Filesystem.Path.CurrentOS as FS
import           System.Environment
import           Text.Printf
import           Text.Taygeta.Tokenizer

-- Types

-- | A pair of tokens.
type Bigram        = (TokenType, TokenType)

-- | This maps between tokens and a list of positions in the document.
type InvertedIndex = M.HashMap TokenType [PositionRange]

-- Conduits and Sinks

-- | Input a sequence of items and convert it into pairs of items occuring next
-- to each other in the sequence.
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

-- | Consume a sequence into a map of bigram frequencies.
countBigrams :: Monad m => C.Sink TokenType m (FreqMap Bigram)
countBigrams = bigrams =$ countFreqsC

-- | Consume a sequence into maps of token and bigram frequencies.
freqs :: Monad m => C.Sink TextPos m (FreqMap TokenType, FreqMap Bigram)
freqs = CL.map snd =$ CU.zipSinks countFreqsC countBigrams

-- | Consume a sequence into an inverted index.
indexTokens :: Monad m => C.Sink TextPos m InvertedIndex
indexTokens = CL.fold alter M.empty
    where alter m (l, t) = M.insertWith mappend t [l] m

-- Consume a sequence returning the number of items in the sequence.
count :: Monad m => C.Sink a m Int
count = CL.fold (\x _ -> x + 1) 0

-- Utilities

-- | Generate frequencies for the items in a list.
countFreqs :: (Ord a, Hashable a) => [a] -> FreqMap a
countFreqs = L.foldl' inc M.empty
    where inc m t = M.insertWith (+) t 1 m

-- | Sort the frequencies in reverse descending order.
sortFreqs :: FreqMap k -> [(k, Int)]
sortFreqs = L.reverse . L.sortBy (comparing snd) . M.toList

-- Reports

-- | Generate a report string of the frequencies.
--
-- The report has these columns:
--
-- * Rank;
-- * Item;
-- * Frequency;
-- * Rank * Frequency; and
-- * Term Frequency (tc(t, d) / max { tc(w, d) : w ∈ d }).
reportFreqs :: (Show a)
            => Int
            -- ^ The maximum frequency of any item in the stream.
            -> [(a, Int)]
            -- ^ The sorted frequency list to generate the report for.
            -> String
            -- ^ The report output as a 'String'.
reportFreqs maxFreq = L.foldr format [] . zip [1..]
    where
        format (r, (a, f)) s =
            let tf :: Double
                tf = fromIntegral f / fromIntegral maxFreq
            in  printf "%4d. %20s %-5d %6d %.4f\n" r (show a) f (r * f) tf ++ s

-- | Generate a keyword in context (KWIC) view of an item in a text.
kwic :: [T.Text]
     -- ^ The input document as a list of 'T.Text'.
     -> Int
     -- ^ The width of the KWICs.
     -> InvertedIndex
     -- ^ The `InvertedIndex' mapping the 'TokenType's to lists of 'PositionRange'.
     -> T.Text
     -- ^ The item to find and highlight in the KWIC.
     -> [T.Text]
     -- ^ The output as a list of 'T.Text'.
kwic textLines width index target =
    map getContext
        . mapMaybe findIndex
        . L.sort
        $ M.lookupDefault [] target index
    where
        csize :: Int
        csize = round $ (fromIntegral width / 2 :: Double)
                      -  fromIntegral (T.length target) / (2 :: Double)

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

-- | Write a newline to STDOUT.
nl :: IO ()
nl = putStrLn ""

-- | Write a header for a file name to STDOUT.
header :: String -> IO ()
header filename = do
    putStrLn filename
    putStrLn . take (length filename) $ repeat '='

-- | Write a token frequency report to STDOUT.
report :: Show a
       => Int
       -- ^ The number of items to include in the report.
       -> FreqMap a
       -- ^ The 'FreqMap' to report on.
       -> IO ()
report limit fqs
    | M.null fqs = return ()
    | otherwise  =
        putStrLn . reportFreqs mf . take limit $ sortFreqs fqs
    where mf = L.maximum $ M.elems fqs

-- | Write a summary of tokens and types to STDOUT.
freqReport :: Int -> FreqMap TokenType -> IO ()
freqReport tokenCount typeFreqs =  printf "Total tokens = %d\n" tokenCount
                                >> printf "Total types  = %d\n" (M.size typeFreqs)

-- | Write a frequency report to STDOUT.
--
-- This reports on the number of items that occur for each given frequency.
freqFreqReport :: FreqMap Int -> IO ()
freqFreqReport fqs
    | M.null fqs = return ()
    | otherwise  =
        putStrLn . reportFreqs mf . L.sortBy (comparing fst) $ M.toList fqs
    where mf = L.maximum $ M.elems fqs

main :: IO ()
main = do
    args <- getArgs
    (filename, mtarget) <- case args of
        (f:t:_) -> return (f, Just t)
        (f:_)   -> return (f, Nothing)
        []      -> fail "You must provide a filename to process."


    (((tfreqs, bfreqs), index), tokenCount) <- C.runResourceT $
           CF.sourceFile (FS.decodeString filename)
        $$ CT.decode CT.utf8
        =$ tokenC
        =$ englishFilter
        =$ numberFilter
        =$ alphaNumericFilter
        =$ CL.map (fmap tokenText)
        =$ CU.zipSinks (CU.zipSinks freqs indexTokens) count

    text <- T.lines `fmap` TIO.readFile filename
    let ffreqs  = countFreqs $ M.elems tfreqs

    header filename              >> nl
    report 20 tfreqs             >> nl
    freqReport tokenCount tfreqs >> nl
    freqFreqReport ffreqs        >> nl
    report 20 bfreqs             >> nl

    mapM_ (putStrLn . T.unpack) $ maybe [] (kwic text 78 index . T.pack) mtarget


