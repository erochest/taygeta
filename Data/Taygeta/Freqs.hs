
module Data.Taygeta.Freqs
    ( FreqMap
    , countFreqsC
    , countFreqs
    ) where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           Text.Taygeta.Types

countFreqs :: (Ord a, Hashable a) => [a] -> FreqMap a
countFreqs = M.fromListWith (+) . flip zip (repeat 1)

countFreqsC :: (Monad m, Ord a, Hashable a) => C.Sink a m (FreqMap a)
countFreqsC = CL.fold inc M.empty
    where inc m t = M.insertWith (+) t 1 m

