{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash #-}

{- |
    Module      :  SDP.Vector
    Copyright   :  (c) Andrey Mulik 2019
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
import SDP.IndexedM
import SDP.Indexed
import SDP.Sort
import SDP.Scan

import Data.Vector ( Vector )
import qualified Data.Vector as V

import Data.Function

import SDP.Unrolled.STUnlist
import SDP.Unrolled.IOUnlist
import SDP.Prim.SArray

import SDP.SortM.Tim

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

{- Nullable, Zip, Scan and Estimate instances. -}

instance Nullable (Vector e)
  where
    isNull = V.null
    lzero  = V.empty

instance Zip Vector
  where
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
    nub   = V.uniq
    
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

instance Split (Vector e) e
  where
    take  = V.take
    drop  = V.drop
    split = V.splitAt
    
    takeWhile = V.takeWhile
    dropWhile = V.dropWhile
    
    spanl  = V.span
    breakl = V.break

instance Bordered (Vector e) Int
  where
    lower   _ = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)
    
    sizeOf = V.length

--------------------------------------------------------------------------------

{- Map, Indexed, KFold and Sort instances. -}

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
    
    (.!) = V.unsafeIndex
    (!?) = (V.!?)
    
    Z  // ascs = toMap ascs
    vs // ascs = vs V.// ascs
    
    (.$) = V.findIndex
    (*$) = toList ... V.findIndices

instance Indexed (Vector e) Int e
  where
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es' = accum (flip const) es ies
      where
        ies  = [ (offset bnds i, e) | (i, e) <- assocs es', inRange bnds i ]
        es   = replicate (size bnds) undefined
        bnds = bounds es'

instance KFold (Vector e) Int e
  where
    kfoldr = V.ifoldr
    kfoldl = V.ifoldl . flip
    
    k_foldl = foldl
    k_foldr = foldr

instance Sort (Vector e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance Thaw (ST s) (Vector e) (STArray# s e) where thaw = fromFoldableM
instance Thaw (ST s) (Vector e) (STUnlist s e) where thaw = fromFoldableM

instance Thaw IO (Vector e) (IOArray# e) where thaw = fromFoldableM
instance Thaw IO (Vector e) (IOUnlist e) where thaw = fromFoldableM

instance Freeze (ST s) (STArray# s e) (Vector e) where freeze = fmap fromList . getLeft
instance Freeze (ST s) (STUnlist s e) (Vector e) where freeze = fmap fromList . getLeft

instance Freeze IO (IOArray# e) (Vector e) where freeze = fmap fromList . getLeft
instance Freeze IO (IOUnlist e) (Vector e) where freeze = fmap fromList . getLeft

--------------------------------------------------------------------------------

done :: STArray# s e -> ST s (Vector e)
done =  freeze

