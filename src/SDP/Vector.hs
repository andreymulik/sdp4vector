{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module SDP.Vector
(
  module SDP.Indexed,
  -- module SDP.Sort,
  
  Vector
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed
-- import SDP.Sort

import Data.Vector ( Vector )
import qualified Data.Vector as V

default ()

--------------------------------------------------------------------------------

instance Estimate (Vector e)
  where
    xs <.=>  n = sizeOf xs <=> n
    xs <==> ys = sizeOf xs <=> sizeOf ys
    
    xs .<  n = sizeOf xs <  n
    xs .>  n = sizeOf xs >  n
    xs .<= n = sizeOf xs <= n
    xs .>= n = sizeOf xs >= n
    xs .== n = sizeOf xs == n
    xs ./= n = sizeOf xs /= n
    
    xs .<.  ys = sizeOf xs < sizeOf ys
    xs .>.  ys = sizeOf xs > sizeOf ys
    xs .<=. ys = sizeOf xs <= sizeOf ys
    xs .>=. ys = sizeOf xs >= sizeOf ys
    xs .==. ys = sizeOf xs == sizeOf ys
    xs ./=. ys = sizeOf xs /= sizeOf ys

instance Zip Vector
  where
    zipWith  = V.zipWith
    zipWith3 = V.zipWith3
    zipWith4 = V.zipWith4
    zipWith5 = V.zipWith5
    zipWith6 = V.zipWith6

-- instance Scan

instance Linear (Vector e) e
  where
    single = V.singleton
    toHead = V.cons
    toLast = V.snoc
    isNull = V.null
    
    listL = V.toList
    lzero = V.empty
    
    head = V.head
    tail = V.tail
    init = V.init
    last = V.last
    nub  = V.uniq
    (++) = (V.++)
    
    partitions ps = fmap fromList . partitions ps . toList
    concatMap   f = V.concatMap f . fromFoldable
    
    fromListN = V.fromListN
    replicate = V.replicate
    partition = V.partition
    fromList  = V.fromList
    reverse   = V.reverse
    
    concat = V.concat . toList
    filter = V.filter

instance Split (Vector e) e
  where
    take = V.take
    drop = V.drop
    
    split = V.splitAt
    
    takeWhile = V.takeWhile
    dropWhile = V.dropWhile
    
    spanl  = V.span
    breakl = V.break

instance Bordered (Vector e) Int e
  where
    lower   _ = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)
    
    sizeOf = V.length

instance Indexed (Vector e) Int e
  where
    assoc' bnds@(l, _) defvalue ascs = accum (flip const) es ies
      where
        ies = [ (i - l, e) | (i, e) <- ascs, inRange bnds i ]
        es  = replicate (size bnds) defvalue
    
    fromIndexed es' = accum (flip const) es ies
      where
        ies  = [ (offset bnds i, e) | (i, e) <- assocs es', inRange bnds i ]
        es   = replicate (size bnds) undefined
        bnds = bounds es'
    
    (!^) = V.unsafeIndex
    (.!) = V.unsafeIndex
    (!?) = (V.!?)
    (!)  = (V.!)
    
    (//) = (V.//)
    
    (.$) = V.findIndex
    (*$) = \ p -> toList . V.findIndices p

instance IFold (Vector e) Int e
  where
    ifoldr = V.ifoldr
    ifoldl = \ f -> V.ifoldl (flip f)
    
    i_foldl = foldl
    i_foldr = foldr

-- instance Sort (Vector e) e {- Soon. -}





