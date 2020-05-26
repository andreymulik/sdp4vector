module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.Vector

import Test.QuickCheck.Instances.Vector ()
import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- common tests
    testProperty "vector-boxed-eq             " eqProp,
    testProperty "vector-boxed-ord            " ordProp,
    testProperty "vector-boxed-lexicographic  " lgoProp,
    
    -- linear tests
    testProperty "vector-boxed-linear-basic   " basicLinearProp,
    testProperty "vector-boxed-linear-decons  " deconstructionLinearProp,
    testProperty "vector-boxed-linear-cons    " constructionLinearProp,
    testProperty "vector-boxed-linear-reverse " reverseProp,
    testProperty "vector-boxed-linear-concat  " concatProp,
    
    -- split test
    testProperty "vector-boxed-split          " splitProp,
    
    -- indexed tests
    testProperty "vector-boxed-indexed-basic  " basicIndexedProp,
    testProperty "vector-boxed-indexed-assoc  " assocIndexedProp,
    testProperty "vector-boxed-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "vector-boxed-sort           " sortProp,
    
    -- estimate test
    testProperty "vector-boxed-estimate       " estimateProp
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




