
-- Some playing with things as I read through *Foundations of Statistical
-- Natural Language Processing* by Christopher Manning and Hinrich Shütze.

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import qualified Filesystem.Path.CurrentOS as FS
-- import           Shelly
import           System.Environment
import           Text.Printf

type Token       = T.Text
type FreqMap     = M.Map Token Int
type FreqFreqMap = M.Map Int Int

tokenize :: FilePath -> IO [Token]
tokenize file =   filter (not . T.null)
              .   map (T.filter isAlphaNum . T.toLower)
              .   T.words
              <$> TIO.readFile file

countFreqs :: (Ord a) => [a] -> M.Map a Int
countFreqs = L.foldl' inc M.empty
    where inc m t = M.insertWith' (+) t 1 m

sortFreqs :: (Ord v) => M.Map k v -> [(k, v)]
sortFreqs = L.reverse . L.sortBy (comparing snd) . M.toList

reportFreqs :: (Show a) => [(a, Int)] -> String
reportFreqs = L.foldl' format [] . L.reverse . zip [1..]
    where
        format s (r, (a, f)) =
            (printf "%4d. %20s %-5d %d\n" r (show a) f (r * f)) ++ s

main :: IO ()
main = do
    tokens <- liftM concat . mapM tokenize =<< getArgs
    let freqs  = countFreqs tokens
    let ffreqs = countFreqs $ M.elems freqs

    -- Top 20 most frequent tokens
    putStrLn . reportFreqs . take 20 . sortFreqs $ freqs
    putStrLn ""

    -- General frequency statistics
    printf "Total tokens = %d\n" (length tokens)
    printf "Total types  = %d\n" (M.size freqs)
    putStrLn ""

    -- Frequency of frequencies
    putStrLn . reportFreqs . L.sortBy (comparing fst) $ M.toList ffreqs
    putStrLn ""



