{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This is an attempt to implement an LDA algorithm in Haskell. Initially,
-- I'll be cribbing off of the [online lda
-- package](http://www.cs.princeton.edu/~blei/downloads/onlineldavb.tar).

module LDA where

import           Data.Array.Repa
import           Data.Array.Repa.Eval
import           Data.Array.Repa.Index
import           Data.Array.Repa.Repr.Unboxed
import           Data.Array.Repa.Algorithms.Matrix
import           Data.Array.Repa.Algorithms.Randomish
import           Debug.Trace
import qualified Data.Vector as V
import           Numeric.Digamma (digamma)
import           Test.Hspec
import           Prelude hiding (map, zipWith)

main :: IO ()
main = hspec $ do
    describe "broadcast" $ do
        it "should have a size of the input vector times the cols" $
            let output :: Array U DIM2 Double
                output = computeS $ broadcast eg1 4
            in  ((6 * 4) ==) . size . extent $ output

        it "should have the same number of rows as the length of the input vector" $
            let output :: Array U DIM2 Double
                output = computeS $ broadcast eg1 4
            in  (6 ==) . row . extent $ output

        it "should equal the hard-coded matrix" $
            let expected = fromListUnboxed
                                (ix2 6 4)
                                [ 0.0001, 0.0001, 0.0001, 0.0001
                                , 0.2,    0.2,    0.2,    0.2
                                , 0.4,    0.4,    0.4,    0.4
                                , 0.6,    0.6,    0.6,    0.6
                                , 0.8,    0.8,    0.8,    0.8
                                , 1.0,    1.0,    1.0,    1.0
                                ]
                actual   :: Array U DIM2 Double
                actual   = computeS $ broadcast eg1 4
            in  foldAllS (&&) True $ zipWith (==) expected actual


    describe "dirichlet expectation" $ do
        it "should work properly for vectors" $
            let expected = fromListUnboxed (ix1 6)
                                              [ -1.00014999e+04, -6.21182423e+00
                                              , -3.48416888e+00, -2.46340355e+00
                                              , -1.88779290e+00, -1.50000000e+00
                                              ]
                actual   = dirichletExpectationS eg1
            in  allInMargin expected actual

        it "should work property for 2D arrays" $
            let expected = fromListUnboxed (ix2 3 2)
                                              [ -9.99529064e+03, -2.62548096e-03
                                              , -1.98416888e+00, -9.63403549e-01
                                              , -1.25000000e+00, -8.62207098e-01
                                              ]
                actual   = dirichletExpectationS eg2
            in  allInMargin expected actual

-- For specs.

eg1 :: Array U DIM1 Double
eg1 = fromListUnboxed (ix1 6) [0.0001, 0.2, 0.4, 0.6, 0.8, 1.0]

eg2 :: Array U DIM2 Double
eg2 = computeS $ reshape (ix2 3 2) eg1

margin :: Double
margin = 0.001

allInMargin :: (Source r1 Double, Source r2 Double, Shape sh)
            => Array r1 sh Double -> Array r2 sh Double -> Bool
allInMargin aa ab =
    foldAllS (&&) True $ zipWith (\a b -> abs (a - b) <= margin) aa ab

-- LDA.
--
-- First some utility types, values, and functions.

type DArray r sh = Array r sh Double

meanChangeThresh :: Double
meanChangeThresh = 0.001

-- Need to transform (Array ... b) by projecting each value the times of the
-- number of columns in (Array ... a). After the projection, zipWith the two
-- arrays over the function.
broadcast :: Source r a => Array r DIM1 a -> Int -> Array D DIM2 a
broadcast a cols = backpermute (extent a :. cols) broadcast' a
    where broadcast' (r :. _) = r

rowwise :: (Source r1 a, Source r2 b)
        => (a -> b -> c)
        -> Array r1 DIM2 a
        -> Array r2 DIM1 b
        -> Array U DIM2 c
rowwise f a b
    | rowCount == size (extent b) = undefined
    | otherwise                         = undefined
    where rowCount = row (extent a)

-- Finally, more core functions.

class Shape sh => Dirichlet sh where
    dirichletExpectationS :: (Source r a, Elt a, Unbox a, Num a, RealFloat a)
                          => Array r sh a -> Array D sh a

instance Dirichlet (Z :. Int) where
    dirichletExpectationS a = map (\x -> digamma x - sa) a
        where sa = digamma (sumAllS a)

instance Dirichlet (Z :. Int :. Int) where
    dirichletExpectationS a = map digamma a -^ (broadcast rowPsi cols)
        where rowPsi = map digamma $ sumS a
              cols   = col (extent a)

