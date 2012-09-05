
-- | This contains types and functions for counting and storing frequencies.
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

-- | Generate frequencies for the items in a list.
countFreqs :: (Ord a, Hashable a) => [a] -> FreqMap a
countFreqs = M.fromListWith (+) . flip zip (repeat 1)

-- | Consume the items from a 'C.Conduit' and output a 'FreqMap' for them.
countFreqsC :: (Monad m, Ord a, Hashable a) => C.Sink a m (FreqMap a)
countFreqsC = CL.fold inc M.empty
    where inc m t = M.insertWith (+) t 1 m

