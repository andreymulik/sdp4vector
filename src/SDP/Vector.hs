{-# LANGUAGE Trustworthy, CPP, MagicHash, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.Vector
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Vector@ provides 'Vector' - immutable lazy boxed vector.
-}
module SDP.Vector
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  
  -- * Vector
  Vector
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Forceable
import SDP.IndexedM
import SDP.Indexed
import SDP.Sort
import SDP.Scan

import qualified Data.Vector.Fusion.Bundle as B
import qualified Data.Vector as V

import Data.Vector.Generic ( stream )
import Data.Vector ( Vector )

import SDP.Unrolled.STUnlist
import SDP.Unrolled.IOUnlist
import SDP.Prim.SArray
import SDP.SortM.Tim

default ()

--------------------------------------------------------------------------------

{- Nullable, Zip, Scan and Estimate instances. -}

instance Nullable (Vector e) where isNull = V.null; lzero = V.empty

#if MIN_VERSION_sdp(0,3,0)
instance Forceable (Vector e) where force = V.force
#endif

instance Zip Vector
  where
    all6 f as bs cs ds es fs = B.and $ B.zipWith6 f
      (stream as) (stream bs) (stream cs) (stream ds) (stream es) (stream fs)
    
    all5 f as bs cs ds es = B.and $ B.zipWith5 f
      (stream as) (stream bs) (stream cs) (stream ds) (stream es)
    
    all4 f as bs cs ds = B.and $ B.zipWith4 f
      (stream as) (stream bs) (stream cs) (stream ds)
    
    all3 f as bs cs = B.and $ B.zipWith3 f
      (stream as) (stream bs) (stream cs)
    
    all2 f as bs = B.and $ B.zipWith f
      (stream as) (stream bs)
    
    any6 f as bs cs ds es fs = B.or $ B.zipWith6 f
      (stream as) (stream bs) (stream cs) (stream ds) (stream es) (stream fs)
    
    any5 f as bs cs ds es = B.or $ B.zipWith5 f
      (stream as) (stream bs) (stream cs) (stream ds) (stream es)
    
    any4 f as bs cs ds = B.or $ B.zipWith4 f
      (stream as) (stream bs) (stream cs) (stream ds)
    
    any3 f as bs cs = B.or $ B.zipWith3 f
      (stream as) (stream bs) (stream cs)
    
    any2 f as bs = B.or $ B.zipWith f (stream as) (stream bs)
    
    zipWith  = V.zipWith
    zipWith3 = V.zipWith3
    zipWith4 = V.zipWith4
    zipWith5 = V.zipWith5
    zipWith6 = V.zipWith6

instance Scan (Vector e) e

instance Estimate (Vector e)
  where
    (<==>) = on (<=>) sizeOf
    (.>.)  = on  (>)  sizeOf
    (.<.)  = on  (<)  sizeOf
    (.<=.) = on  (<=) sizeOf
    (.>=.) = on  (>=) sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (Vector e) e
  where
    single = V.singleton
    toHead = V.cons
    toLast = V.snoc
    
    listL = V.toList
    head  = V.head
    tail  = V.tail
    init  = V.init
    last  = V.last
#if MIN_VERSION_vector(0,12,0)
    nub   = V.uniq
#endif
    
    (!^) = V.unsafeIndex
    (++) = (V.++)
    
    write es = (es //) . single ... (,)
    
    partitions ps = fmap fromList . partitions ps . toList
    concatMap   f = V.concatMap f . fromFoldable
    
    fromListN = V.fromListN
    replicate = V.replicate
    partition = V.partition
    fromList  = V.fromList
    reverse   = V.reverse
    
    concat = V.concat . toList
    filter = V.filter
    
#if !MIN_VERSION_sdp(0,3,0)
    force = V.force
#endif
    
    ofoldl = V.ifoldl . flip
    ofoldr = V.ifoldr
    
    o_foldl = foldl
    o_foldr = foldr
#if !MIN_VERSION_sdp(0,3,0)
instance Split (Vector e) e
  where
#endif
    take  = V.take
    drop  = V.drop
    split = V.splitAt
    
    takeWhile = V.takeWhile
    dropWhile = V.dropWhile
    
    spanl  = V.span
    breakl = V.break

instance Bordered (Vector e) Int
  where
    lower    _ = 0
    sizeOf     = V.length
    upper   es = V.length es - 1
    bounds  es = (0, V.length es - 1)
    rebound es = \ bnds -> size bnds `take` es

--------------------------------------------------------------------------------

{- Map, Indexed and Sort instances. -}

instance Map (Vector e) Int e
  where
    toMap ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    toMap' defvalue ascs = isNull ascs ? Z $ assoc' (l, u) defvalue ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    Z  // ascs = toMap ascs
    vs // ascs = vs V.// ascs
    
    (*$) = toList ... V.findIndices
    (.!) = V.unsafeIndex
    (.$) = V.findIndex
    (!?) = (V.!?)
    
    kfoldl = ofoldl
    kfoldr = ofoldr

instance Indexed (Vector e) Int e
  where
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es' = accum (flip const) es ies
      where
        ies  = [ (offset bnds i, e) | (i, e) <- assocs es', inRange bnds i ]
        es   = replicate (size bnds) undefined
        bnds = bounds es'

instance Sort (Vector e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'
    
    sortedBy f = sortedBy f . listL

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance Thaw (ST s) (Vector e) (STArray# s e) where thaw = fromFoldableM
instance Thaw (ST s) (Vector e) (STUnlist s e) where thaw = fromFoldableM

instance (MonadIO io) => Thaw io (Vector e) (MIOArray# io e) where thaw = fromFoldableM
instance (MonadIO io) => Thaw io (Vector e) (MIOUnlist io e) where thaw = fromFoldableM

instance Freeze (ST s) (STArray# s e) (Vector e) where freeze = fmap fromList . getLeft
instance Freeze (ST s) (STUnlist s e) (Vector e) where freeze = fmap fromList . getLeft

instance (MonadIO io) => Freeze io (MIOArray# io e) (Vector e) where freeze = fmap fromList . getLeft
instance (MonadIO io) => Freeze io (MIOUnlist io e) (Vector e) where freeze = fmap fromList . getLeft

--------------------------------------------------------------------------------

done :: STArray# s e -> ST s (Vector e)
done =  freeze



