{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This is an attempt to implement an LDA algorithm in Haskell. Initially,
-- I'll be cribbing off of the [online lda
-- package](http://www.cs.princeton.edu/~blei/downloads/onlineldavb.tar).

-- http://hackage.haskell.org/package/base-4.5.1.0
-- http://docs.scipy.org/doc/numpy/reference/
-- http://docs.scipy.org/doc/scipy/reference/
-- http://hackage.haskell.org/package/statistics
-- http://hackage.haskell.org/package/math-functions-0.1.1.1
-- http://hackage.haskell.org/package/vector-0.9.1
-- http://hackage.haskell.org/package/vector-algorithms-0.5.4.1
-- http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hmatrix

module Main where

import qualified Data.Vector as V
import           Numeric.Digamma (digamma)

-- TODO: It would be nice to be able to push requirements about operators onto
-- the type system. In other words, I want to make sure that (M x*y) - (M x*1).
-- I may be able to use GADTs for that. (Or maybe not. That might require
-- Church encoding the numbers, which seems awkward and unnecessary for this.)

-- | I can't find a simple matrix type that doesn't require me to install
-- everything in the universe. Here is one. (I borrowed this from the
-- statistics-dirichlet package).
data Matrix a = M
    { mRows :: !Int
    , mCols :: !Int
    , mData :: !(V.Vector a)      -- ^ The data is stored in row-major order.
    } deriving (Eq, Show)

instance Functor Matrix where
    fmap f m = m { mData = fmap f (mData m) }

meanChangeThresh :: Double
meanChangeThresh = 0.001

(><) :: Int -> Int -> [a] -> Matrix a
rows >< cols = M rows cols . V.fromListN (rows * cols)

fromVector :: V.Vector a -> Matrix a
fromVector v = M (V.length v) 1 v

row :: Matrix a -> Int -> V.Vector a
row M{..} i = V.slice (i * mRows) mRows mData 

col :: Matrix a -> Int -> V.Vector a
col M{..} i = V.backpermute mData . V.generate mRows $ \x -> x * mCols + i

columize :: Matrix a -> Matrix a
columize m = m { mCols = 1 }

-- | This maps a function over the rows in the matrix.
rowmap :: (V.Vector a -> b) -> Matrix a -> Matrix b
rowmap f m = fromVector . V.map (f . row m) $ V.fromList [0 .. (mRows m - 1)]

dirichletExpectation :: Matrix Double -> Matrix Double
dirichletExpectation m@(M 1 0 matrix) = fmap (\x -> digamma x - s) m
    where s = digamma (V.sum matrix)
dirichletExpectation m@M{..} = fromVector $ V.map digamma mData
    where sum = rowmap V.sum m

main :: IO ()
main = putStrLn "LDA"


