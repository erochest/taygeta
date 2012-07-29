{-# LANGUAGE OverloadedStrings #-}

-- Some playing with things as I read through *Foundations of Statistical
-- Natural Language Processing* by Christopher Manning and Hinrich Shütze.

module Main where

import           Data.Char
import           Data.Hashable
import qualified Data.List as L
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import qualified Filesystem.Path.CurrentOS as FS
-- import           Shelly
import           System.Environment
import           Text.Printf

type Token         = T.Text
type Location      = Int
type Bigram        = (Token, Token)
type FreqMap a     = M.HashMap a Int
type InvertedIndex = M.HashMap Token [Location]

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

        notEmpty = not . T.null . snd

normalize :: Token -> Token
normalize = T.filter isAlphaNum . T.toLower

bigrams :: [a] -> [(a, a)]
bigrams (a: (as@(b:_))) = (a, b) : bigrams as
bigrams _               = []

countFreqs :: (Ord a, Hashable a) => [a] -> FreqMap a
countFreqs = L.foldl' inc M.empty
    where inc m t = M.insertWith (+) t 1 m

sortFreqs :: FreqMap k -> [(k, Int)]
sortFreqs = L.reverse . L.sortBy (comparing snd) . M.toList

reportFreqs :: (Show a) => [(a, Int)] -> String
reportFreqs = L.foldr format [] . L.reverse . zip [1..]
    where
        format (r, (a, f)) s =
            (printf "%4d. %20s %-5d %d\n" r (show a) f (r * f)) ++ s

indexTokens :: [(Location, Token)] -> InvertedIndex
indexTokens = L.foldl' alter M.empty
    where alter m (l, t) = M.insertWith mappend t [l] m

kwic :: T.Text -> Int -> InvertedIndex -> T.Text -> [T.Text]
kwic text width index target =
    map getContext . L.sort $ M.lookupDefault [] target index
    where
        csize :: Int
        csize   = round $ (fromIntegral width / 2) - (fromIntegral $ T.length target) / 2

        rmnl '\n' = ' '
        rmnl '\r' = ' '
        rmnl c    = c

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

tokenFreqReport :: Int -> FreqMap Token -> IO ()
tokenFreqReport count = putStrLn . reportFreqs . take count . sortFreqs

freqReport :: [Token] -> FreqMap Token -> IO ()
freqReport tokens freqs = do
    printf "Total tokens = %d\n" (length tokens)
    printf "Total types  = %d\n" (M.size freqs)

freqFreqReport :: FreqMap Int -> IO ()
freqFreqReport = 
    putStrLn . reportFreqs . L.sortBy (comparing fst) . M.toList

bigramReport :: Int -> FreqMap Bigram -> IO ()
bigramReport count = putStrLn . reportFreqs . take count . sortFreqs


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
    tokenFreqReport 20 freqs    >> nl
    freqReport tokens' freqs    >> nl
    freqFreqReport ffreqs       >> nl
    bigramReport 20 bfreqs      >> nl

    mapM_ (putStrLn . T.unpack) $ maybe [] (kwic text 78 index . T.pack) mtarget


