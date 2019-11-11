{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    Module      :  SDP.Vector
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Vector@ provides 'Vector' - immutable lazy boxed vector.
-}
module SDP.Vector
(
  -- * Exports
  module SDP.Indexed,
  -- module SDP.Sort,
  
  -- * Vector
  Vector
)
where

import Prelude ()
import SDP.SafePrelude

import Control.Monad.ST ( runST, ST )

import SDP.IndexedM
import SDP.Indexed
import SDP.Sort
import SDP.Scan

import Data.Vector ( Vector )
import qualified Data.Vector as V

import SDP.Array.ST
import SDP.Unrolled.ST
import SDP.Unrolled.STUnlist

import SDP.Array.IO
import SDP.Unrolled.IO
import SDP.Unrolled.IOUnlist

import SDP.SortM.Tim

default ()

--------------------------------------------------------------------------------

{- Zip, Scan and Estimate instances. -}

instance Zip Vector
  where
    zipWith  = V.zipWith
    zipWith3 = V.zipWith3
    zipWith4 = V.zipWith4
    zipWith5 = V.zipWith5
    zipWith6 = V.zipWith6

instance Scan Vector
  where
    scanl = V.scanl
    scanr = V.scanr
    
    scanl' = V.scanl'
    scanl1 = V.scanl1
    scanr1 = V.scanr1

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

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

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

--------------------------------------------------------------------------------

{- Indexed, IFold and Sort instances. -}

instance Indexed (Vector e) Int e
  where
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es' = accum (flip const) es ies
      where
        ies  = [ (offset bnds i, e) | (i, e) <- assocs es', inRange bnds i ]
        es   = replicate (size bnds) undefined
        bnds = bounds es'
    
    (!^) = V.unsafeIndex
    (.!) = V.unsafeIndex
    (!?) = (V.!?)
    (!)  = (V.!)
    
    Z  // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    vs // ascs = vs V.// ascs
    
    (.$) = V.findIndex
    (*$) = \ p -> toList . V.findIndices p

instance IFold (Vector e) Int e
  where
    ifoldr = V.ifoldr
    ifoldl = \ f -> V.ifoldl (flip f)
    
    i_foldl = foldl
    i_foldr = foldr

instance Sort (Vector e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance Thaw (ST s) (Vector e) (STArray    s Int e) where thaw = fromFoldableM
instance Thaw (ST s) (Vector e) (STUnlist   s     e) where thaw = fromFoldableM
instance Thaw (ST s) (Vector e) (STUnrolled s Int e) where thaw = fromFoldableM

instance Thaw IO (Vector e) (IOArray    Int e) where thaw = fromFoldableM
instance Thaw IO (Vector e) (IOUnlist       e) where thaw = fromFoldableM
instance Thaw IO (Vector e) (IOUnrolled Int e) where thaw = fromFoldableM



instance Freeze (ST s) (STArray    s Int e) (Vector e) where freeze = fmap fromList . getLeft
instance Freeze (ST s) (STUnlist   s     e) (Vector e) where freeze = fmap fromList . getLeft
instance Freeze (ST s) (STUnrolled s Int e) (Vector e) where freeze = fmap fromList . getLeft

instance Freeze IO (IOArray    Int e) (Vector e) where freeze = fmap fromList . getLeft
instance Freeze IO (IOUnlist       e) (Vector e) where freeze = fmap fromList . getLeft
instance Freeze IO (IOUnrolled Int e) (Vector e) where freeze = fmap fromList . getLeft

--------------------------------------------------------------------------------

done :: STArray s Int e -> ST s (Vector e)
done =  freeze



