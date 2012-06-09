{-# LANGUAGE OverloadedStrings #-}

-- Some playing with things as I read through *Foundations of Statistical
-- Natural Language Processing* by Christopher Manning and Hinrich Shütze.

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (maybe, mapMaybe)
import           Data.Monoid
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import qualified Filesystem.Path.CurrentOS as FS
-- import           Shelly
import           System.Environment
import           Text.Printf
import           Debug.Trace

type Token         = T.Text
type Location      = Int
type Bigram        = (Token, Token)
type FreqMap a     = M.Map a Int
type InvertedIndex = M.Map Token [Location]

tokenizeFile :: FilePath -> IO [(Location, Token)]
tokenizeFile = fmap tokenize . TIO.readFile

tokenize :: T.Text -> [(Location, Token)]
tokenize input = filter notEmpty . map (fmap normalize) $ tokenize' input 0
    where
        tokenize' inp i1 =
            let (s, inp')  = T.span isSpace inp
                loc        = i1 + T.length s
                (t, inp'') = T.break isSpace inp'
                loc'       = loc + T.length t
            in  if T.null t
                    then []
                    else ((loc, t) : tokenize' inp'' loc')

        notEmpty (_, t) | T.null t  = False
                        | otherwise = True

normalize :: Token -> Token
normalize = T.filter isAlphaNum . T.toLower

bigrams :: [a] -> [(a, a)]
bigrams (a: (as@(b:_))) = (a, b) : bigrams as
bigrams _             = []

countFreqs :: (Ord a) => [a] -> M.Map a Int
countFreqs = L.foldl' inc M.empty
    where inc m t = M.insertWith' (+) t 1 m

sortFreqs :: FreqMap k -> [(k, Int)]
sortFreqs = L.reverse . L.sortBy (comparing snd) . M.toList

reportFreqs :: (Show a) => [(a, Int)] -> String
reportFreqs = L.foldl' format [] . L.reverse . zip [1..]
    where
        format s (r, (a, f)) =
            (printf "%4d. %20s %-5d %d\n" r (show a) f (r * f)) ++ s

indexTokens :: [(Location, Token)] -> InvertedIndex
indexTokens = L.foldl' alter M.empty
    where
        alter m (l, t) = M.alter (alter' l) t m

        alter' l (Just ls) = Just (l:ls)
        alter' l Nothing   = Just [l]

kwic :: T.Text -> Int -> InvertedIndex -> T.Text -> [T.Text]
kwic text width index target =
    map getContext . L.sort $ M.findWithDefault [] target index
    where
        csize :: Int
        csize   = round $ (fromIntegral width / 2) - (fromIntegral $ T.length target) / 2

        rmnl '\n' = ' '
        rmnl '\r' = ' '
        rmnl c    = c

        target' = T.toLower target'
        text'   = T.map rmnl text

        -- Umm. There has to be a better way.
        getContext l =
            let start    = max 0 $ l - csize
                c        = T.take width $ T.drop start text'
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

tokenFreqReport :: FreqMap Token -> IO ()
tokenFreqReport =
    -- Top 20 most frequent tokens
    putStrLn . reportFreqs . take 20 . sortFreqs

freqReport :: [Token] -> FreqMap Token -> IO ()
freqReport tokens freqs = do
    -- General frequency statistics
    printf "Total tokens = %d\n" (length tokens)
    printf "Total types  = %d\n" (M.size freqs)

freqFreqReport :: FreqMap Int -> IO ()
freqFreqReport = 
    -- Frequency of frequencies
    putStrLn . reportFreqs . L.sortBy (comparing fst) . M.toList

bigramReport :: FreqMap Bigram -> IO ()
bigramReport =
    -- Frequency of bigrams
    putStrLn . reportFreqs . take 20 . sortFreqs


main :: IO ()
main = do
    args <- getArgs
    (filename, mtarget) <- case args of
        (f:t:_) -> return (f, Just t)
        (f:_)   -> return (f, Nothing)
        []      -> fail "You must provide a filename to process."

    text <- TIO.readFile filename
    let tokens  = tokenize text
        tokens' = map snd tokens

        freqs   = countFreqs tokens'
        ffreqs  = countFreqs $ M.elems freqs

        bigs    = bigrams tokens'
        bfreqs  = countFreqs bigs

        index   = indexTokens tokens

    header filename             >> nl
    tokenFreqReport freqs       >> nl
    freqReport tokens' freqs    >> nl
    freqFreqReport ffreqs       >> nl
    bigramReport bfreqs         >> nl

    mapM_ (putStrLn . T.unpack) $ maybe [] (kwic text 78 index . T.pack) mtarget


