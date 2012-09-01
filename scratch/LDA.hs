{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This is an attempt to implement an LDA algorithm in Haskell. Initially,
-- I'll be cribbing off of the [online lda
-- package](http://www.cs.princeton.edu/~blei/downloads/onlineldavb.tar).

module LDA where

import           Control.Applicative
import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Primitive
import           Control.Monad.Trans.Resource
import           Data.Array.Repa
import           Data.Array.Repa.Eval
import           Data.Array.Repa.Index
import           Data.Array.Repa.Repr.Unboxed
import           Data.Array.Repa.Algorithms.Matrix
import           Data.Array.Repa.Algorithms.Randomish
import           Data.Conduit ((=$=), (=$), ($=), ($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Filesystem as CFS
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import           Data.Taygeta.Freqs
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VM
import           Debug.Trace
import qualified Filesystem.Path.CurrentOS as FS
import           Numeric.Digamma (digamma)
import           Prelude hiding (map, zipWith)
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.Random.MWC
import           System.Random.MWC.Distributions
import           Test.Hspec
import           Text.Taygeta.Tokenizer

main :: IO ()
main = hspec $ do
    describe "broadcast" $ do
        it "should have a size of the input vector times the cols" $
            let output :: Matrix U
                output = computeS $ broadcast eg1 4
            in  ((6 * 4) ==) . size . extent $ output

        it "should have the same number of rows as the length of the input vector" $
            let output :: Matrix U
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
                actual   :: Matrix U
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

eg2 :: Matrix U
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
type Matrix r    = Array r DIM2 Double

meanChangeThresh :: Double
meanChangeThresh = 0.001

-- | Need to transform (Array ... b) by projecting each value the times of the
-- number of columns in (Array ... a). After the projection, zipWith the two
-- arrays over the function.
broadcast :: Source r a => Array r DIM1 a -> Int -> Array D DIM2 a
broadcast a cols = backpermute (extent a :. cols) broadcast' a
    where broadcast' (r :. _) = r

-- Parsing

ldaTokenizeC :: (Monad m, C.MonadThrow m) => C.Conduit T.Text m T.Text
ldaTokenizeC =   tokenC
             =$= numberFilter
             =$= englishFilter
             =$= alphaNumericFilter
             =$= englishStopListFilter
             =$= CL.map (tokenText . snd)

ldaTokenizeFile :: FS.FilePath -> IO [T.Text]
ldaTokenizeFile input = C.runResourceT $
       CB.sourceFile (FS.encodeString input)
    $= CT.decode CT.utf8 
    $$ ldaTokenizeC
    =$ CL.consume

ldaTokenizeText :: T.Text -> Either SomeException [T.Text]
ldaTokenizeText input = runIdentity . runExceptionT $
    CL.sourceList [input] $$ ldaTokenizeC =$ CL.consume

-- Indexing/Vectoring

type IndexMap a = M.HashMap a Int

indexList :: (Ord a, Hashable a) => IndexMap a -> [a] -> IndexMap a
indexList = L.foldl' (\idx x -> M.insertWith (flip const) x (M.size idx) idx)

getRecursiveFiles :: FS.FilePath -> IO [FS.FilePath]
getRecursiveFiles root = do
    names' <- getDirectoryContents $ FS.encodeString root
    let names = L.map ((root FS.</>) . FS.decodeString)
              $ L.filter (`notElem` [".", ".."]) names'
    paths  <- forM names $ \name -> do
        isDirectory <- doesDirectoryExist (FS.encodeString name)
        if isDirectory
            then getRecursiveFiles name
            else return [name]
    return $ concat paths

readContents :: FS.FilePath -> IO [(FS.FilePath, T.Text)]
readContents dirname = do
    files <- getRecursiveFiles dirname
    forM files $ \fp ->
        (,) fp <$> TIO.readFile (FS.encodeString fp)

readFreqMaps :: FS.FilePath -> IO [(FS.FilePath, FreqMap T.Text)]
readFreqMaps dirname = do
    files <- getRecursiveFiles dirname
    forM files $ \fp ->
        fmap ((,) fp) $ C.runResourceT
               $   CFS.sourceFile fp $= CT.decode CT.utf8
               $$  ldaTokenizeC =$ countFreqsC

makeFreqMaps :: T.Text -> Either SomeException (FreqMap T.Text)
makeFreqMaps text = runIdentity . runExceptionT $
    CL.sourceList [text] $$ ldaTokenizeC =$ countFreqsC

makeTokenIndex :: [FreqMap T.Text] -> IndexMap T.Text
makeTokenIndex = indexList M.empty . L.concatMap M.keys

makeVector :: IndexMap T.Text -> FreqMap T.Text -> VU.Vector Double
makeVector idx freqs = VU.create $ do
    v <- VM.replicate (M.size idx) (0.0 :: Double)
    foldM setToken v (M.keys freqs)
    where
        setToken v k = 
            case M.lookup k idx of
                Nothing -> return v
                Just i  -> VM.write v i (1.0 :: Double) >> return v

-- Finally, more core functions.

class Shape sh => Dirichlet sh where
    dirichletExpectationS :: (Source r a, Elt a, Unbox a, Num a, RealFloat a)
                          => Array r sh a -> Array D sh a

instance Dirichlet (Z :. Int) where
    dirichletExpectationS a = map (\x -> digamma x - sa) a
        where sa = digamma (sumAllS a)

instance Dirichlet (Z :. Int :. Int) where
    dirichletExpectationS a = map digamma a -^ broadcast rowPsi cols
        where rowPsi = map digamma $ sumS a
              cols   = col (extent a)

data LDA = LDA
    { topicCount :: Int
    , tokenIndex :: IndexMap T.Text
    , docCount   :: Int
    , alpha      :: Double
    , eta        :: Double
    , tau0       :: Double
    , kappa      :: Double
    } deriving (Show)

initLda :: Int -> FS.FilePath -> Double -> Double -> Double -> Double -> IO LDA
initLda k dirname alpha eta tau0 kappa = do
    freqs <- L.map snd <$> readFreqMaps dirname
    return $ LDA k (makeTokenIndex freqs) (L.length freqs) alpha eta tau0 kappa

-- | The distributions returned are:
--
-- * random values over the distribution;
-- * the Dirichlet expectation for the distribution; and
-- * the exponential of of values in the Dir expectation.
initDistributions :: LDA -> IO (Matrix D, Matrix D, Matrix D)
initDistributions LDA{..} = do
        l <- gammaList 100.0 (1.0 / 100.0) (w * topicCount)
        let d  = fromListUnboxed (ix2 topicCount w) l
            de = dirichletExpectationS d
            ex = map exp de
        return (delay d, de, ex)
    where w = M.size tokenIndex

gammaList :: Double -> Double -> Int -> IO [Double]
gammaList shape scale len = do
    gen  <- create
    replicateM len (gamma shape scale gen)

e :: Source r Double => LDA -> Matrix r -> Matrix D -> Matrix D -> FS.FilePath -> IO ()
e LDA{..} lmbd de deExp dirname = do
    freqs  <- readFreqMaps dirname

    -- batch variational distribution q(theta|gamma) for the mini-batch.
    let gextent = ix2 topicCount $ length freqs
        gsize   = topicCount * length freqs
    bgamma <-  delay . fromListUnboxed gextent
           <$> gammaList 100.0 (1.0 / 100.0) gsize
    let bltheta = dirichletExpectationS bgamma
        bexplth = map exp bltheta
        sstats  = delay . fromListUnboxed gextent $ L.replicate gsize (0.0 :: Double)
        ifreqs  = Prelude.zip ([0..] :: [Int]) freqs

    forM_ ifreqs $ \(i, (fp, fq)) ->
        let dv      = fromUnboxed (ix1 (M.size tokenIndex))
                                  (makeVector tokenIndex fq)
            sl      = Any :. i :. All
            dg      = slice bgamma  sl
            dt      = slice bltheta sl
            de      = slice bexplth sl
            db      = zipWith (*) (slice deExp sl) dv
            phiNorm = (1e-100) + sumAllS (zipWith (*) de db)

        in  return ()

