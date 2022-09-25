{-# LANGUAGE Trustworthy, CPP, MagicHash, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.Vector.Unboxed
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Vector.Unboxed@ provides 'Vector' - immutable strict unboxed vector.
    
    This module uses the Unbox and Unboxed classes. Looking at similar names
    and, in general, general purpose, they are fundamentally different:
    
    Despite similar names, classes are very different:
    * 'Unboxed' is a low-level interface that generalizes access to data.
    * 'Unbox' is a service class that combines Vector and MVector.
-}
module SDP.Vector.Unboxed
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Scan,
  
  -- * Vector
  Unbox, Vector
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Scan

import SDP.ByteList.STUblist
import SDP.ByteList.IOUblist

import SDP.Prim.SBytes
import SDP.SortM.Tim

import Data.Vector.Unboxed ( Vector, Unbox )
import qualified Data.Vector.Unboxed as V

default ()

--------------------------------------------------------------------------------

{- Nullable, Forceable, Scan and Estimate instances. -}

instance Unbox e => Nullable (Vector e)
  where
    isNull = V.null
    lzero  = V.empty

#if MIN_VERSION_sdp(0,3,0)
instance Unbox e => Forceable (Vector e)
  where
    force = V.force
#endif

instance Unbox e => Estimate (Vector e)
  where
    (<==>) = on (<=>) sizeOf
    (.>=.) = on (>=)  sizeOf
    (.<=.) = on (<=)  sizeOf
    (.>.)  = on  (>)  sizeOf
    (.<.)  = on  (<)  sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf
    
#if MIN_VERSION_sdp(0,3,0)
    sizeOf = V.length
#endif

--------------------------------------------------------------------------------

{- Semigroup instance. -}

#if !MIN_VERSION_vector(0,12,0)
instance Unbox e => Semigroup (Vector e)
  where
    (<>) = (++)
#endif

--------------------------------------------------------------------------------

{- Bordered instance. -}

instance Unbox e => Bordered (Vector e) Int
  where
    lower   _ = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)
    
#if MIN_VERSION_sdp(0,3,0)
    rebound = V.take . size
#else
    sizeOf  = V.length
#endif

--------------------------------------------------------------------------------

{- Linear and Split instances. -}

instance Unbox e => Linear (Vector e) e
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
    
    write es = (es V.//) . single ... (,)
    
    concatMap f = concat . foldr ((:) . f) []
    
    fromListN = V.fromListN
    replicate = V.replicate
    partition = V.partition
    fromList  = V.fromList
    reverse   = V.reverse
    
    concat = V.concat . toList
    filter = V.filter
    
#if MIN_VERSION_sdp(0,3,0)
    sfoldl = V.foldl
    sfoldr = V.foldr
#else
    partitions ps = fmap fromList . partitions ps . listL
    
    o_foldl = V.foldl
    o_foldr = V.foldr
    
    force = V.force
#endif
    
    ofoldl = V.ifoldl . flip
    ofoldr = V.ifoldr
#if !MIN_VERSION_sdp(0,3,0)
instance Unbox e => Split (Vector e) e
  where
    prefix p = V.foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = V.foldl (\ c e -> p e ? c + 1 $ 0) 0
    
    breakl = V.break
    spanl  = V.span
#endif
    
    takeWhile = V.takeWhile
    dropWhile = V.dropWhile
    
    take  = V.take
    drop  = V.drop
    split = V.splitAt

--------------------------------------------------------------------------------

{- Map and Indexed instances. -}

instance (Unboxed e, Unbox e) => Map (Vector e) Int e
  where
    toMap ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    toMap' e ascs = isNull ascs ? Z $ assoc' (l, u) e ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    (.!) = V.unsafeIndex
    (!?) = (V.!?)
    
    Z  // ascs = toMap ascs
    vs // ascs = vs V.// ascs
    
    (.$) = V.findIndex
    (*$) = listL ... V.findIndices
    
    kfoldl = ofoldl
    kfoldr = ofoldr

instance (Unboxed e, Unbox e) => Indexed (Vector e) Int e
  where
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    assoc' bnds e ascs = runST $ fromAssocs' bnds e ascs >>= done
    
    fromIndexed es = defaultBounds (sizeOf es) `assoc`
      [ (offsetOf es i, e) | (i, e) <- assocs es, indexIn es i ]

--------------------------------------------------------------------------------

{- Scan and Sort instances. -}

instance Unbox e => Scan (Vector e) e

instance (Unboxed e, Unbox e) => Sort (Vector e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'
    
    sortedBy f = sortedBy f . listL

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance (Unboxed e, Unbox e) => Thaw (ST s) (Vector e) (STBytes# s e) where thaw = fromIndexed'
instance (Unboxed e, Unbox e) => Thaw (ST s) (Vector e) (STUblist s e) where thaw = fromIndexed'

instance (MonadIO io, Unboxed e, Unbox e) => Thaw io (Vector e) (MIOBytes# io e) where thaw = fromIndexed'
instance (MonadIO io, Unboxed e, Unbox e) => Thaw io (Vector e) (MIOUblist io e) where thaw = fromIndexed'

instance (Unboxed e, Unbox e) => Freeze (ST s) (STBytes# s e) (Vector e) where freeze = fmap fromList . getLeft
instance (Unboxed e, Unbox e) => Freeze (ST s) (STUblist s e) (Vector e) where freeze = fmap fromList . getLeft

instance (MonadIO io, Unboxed e, Unbox e) => Freeze io (MIOBytes# io e) (Vector e) where freeze = fmap fromList . getLeft
instance (MonadIO io, Unboxed e, Unbox e) => Freeze io (MIOUblist io e) (Vector e) where freeze = fmap fromList . getLeft

--------------------------------------------------------------------------------

done :: (Unboxed e, Unbox e) => STBytes# s e -> ST s (Vector e)
done =  freeze

