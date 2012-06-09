
-- Some playing with things as I read through *Foundations of Statistical
-- Natural Language Processing* by Christopher Manning and Hinrich Shütze.

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import qualified Filesystem.Path.CurrentOS as FS
-- import           Shelly
import           System.Environment
import           Text.Printf

type Token       = T.Text
type Location    = Int
type Bigram      = (Token, Token)
type FreqMap a   = M.Map a Int

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

main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \filename -> do
        text <- TIO.readFile filename
        let tokens  = tokenize text
            tokens' = map snd tokens

            freqs   = countFreqs tokens'
            ffreqs  = countFreqs $ M.elems freqs

            bigs    = bigrams tokens'
            bfreqs  = countFreqs bigs

        header filename             >> nl
        tokenFreqReport freqs       >> nl
        freqReport tokens' freqs    >> nl
        freqFreqReport ffreqs       >> nl
        bigramReport bfreqs         >> nl

    where
        nl = putStrLn ""

        header filename = do
            putStrLn filename
            putStrLn . take (length filename) $ repeat '='

        tokenFreqReport =
            -- Top 20 most frequent tokens
            putStrLn . reportFreqs . take 20 . sortFreqs

        freqReport :: [Token] -> FreqMap Token -> IO ()
        freqReport tokens freqs = do
            -- General frequency statistics
            printf "Total tokens = %d\n" (length tokens)
            printf "Total types  = %d\n" (M.size freqs)

        freqFreqReport = 
            -- Frequency of frequencies
            putStrLn . reportFreqs . L.sortBy (comparing fst) . M.toList

        bigramReport =
            -- Frequency of bigrams
            putStrLn . reportFreqs . take 20 . sortFreqs


