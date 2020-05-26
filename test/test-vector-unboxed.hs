module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.Vector.Unboxed

import Test.QuickCheck.Instances.Vector ()
import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- common tests
    testProperty "vector-unboxed-eq             " eqProp,
    testProperty "vector-unboxed-ord            " ordProp,
    testProperty "vector-unboxed-lexicographic  " lgoProp,
    
    -- linear tests
    testProperty "vector-unboxed-linear-basic   " basicLinearProp,
    testProperty "vector-unboxed-linear-decons  " deconstructionLinearProp,
    testProperty "vector-unboxed-linear-cons    " constructionLinearProp,
    testProperty "vector-unboxed-linear-reverse " reverseProp,
    testProperty "vector-unboxed-linear-concat  " concatProp,
    
    -- split test
    testProperty "vector-unboxed-split          " splitProp,
    
    -- indexed tests
    testProperty "vector-unboxed-indexed-basic  " basicIndexedProp,
    testProperty "vector-unboxed-indexed-assoc  " assocIndexedProp,
    testProperty "vector-unboxed-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "vector-unboxed-sort           " sortProp,
    
    -- estimate test
    testProperty "vector-unboxed-estimate       " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqProp :: TestEq (Vector Int)
eqProp =  eqTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordProp :: TestOrd (Vector Int)
ordProp =  ordTest

lgoProp :: Long (Vector Int) -> Long (Vector Int) -> Bool
lgoProp (Long xs) (Long ys) = lexicographicOrdTest xs ys

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp          :: Int -> Vector Int -> Bool
basicLinearProp          =  basicLinearTest

deconstructionLinearProp :: Vector Int -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp   :: Int -> Vector Int -> Bool
constructionLinearProp   =  constructionLinearTest

reverseProp              :: Vector Int -> Bool
reverseProp              =  reverseTest

replicateProp            :: TestLinear (Vector Int) Int
replicateProp            =  replicateTest

concatProp               :: (Vector Int) -> Bool
concatProp               =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: TestSplit (Vector Int)
splitProp =  splitTest

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed1 Vector Int Int
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed1 Vector Int Int
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed1 Vector Int Int
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Medium (Vector Int) -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (Vector Int)
estimateProp =  estimateTest




