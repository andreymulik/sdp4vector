{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    Module      :  SDP.Vector.Unboxed
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
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
  
  Unbox,
  
  -- * Vector
  Vector
)
where

import Prelude ()
import SDP.SafePrelude

import Control.Monad.ST ( runST, ST )

import Data.Vector.Unboxed ( Vector, Unbox )
import qualified Data.Vector.Unboxed as V

import SDP.IndexedM
import SDP.Indexed
import SDP.Sort

import SDP.SortM.Tim
import SDP.Unboxed

import SDP.Bytes.ST
import SDP.ByteList.ST
import SDP.ByteList.STUblist

import SDP.Bytes.IO
import SDP.ByteList.IO
import SDP.ByteList.IOUblist

default ()

--------------------------------------------------------------------------------

{- Estimate instance. -}

instance (Unbox e, Unboxed e) => Estimate (Vector e)
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

instance (Unbox e) => Linear (Vector e) e
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
    
    partitions ps = fmap fromList . partitions ps . listL
    concatMap   f = concat . foldr (\ x y -> f x : y) []
    
    fromListN = V.fromListN
    replicate = V.replicate
    partition = V.partition
    fromList  = V.fromList
    reverse   = V.reverse
    
    concat = V.concat . toList
    filter = V.filter

instance (Unbox e) => Split (Vector e) e
  where
    take = V.take
    drop = V.drop
    
    split = V.splitAt
    
    takeWhile = V.takeWhile
    dropWhile = V.dropWhile
    
    spanl  = V.span
    breakl = V.break
    
    prefix p = V.foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = V.foldl (\ c e -> p e ? c + 1 $ 0) 0

instance (Unbox e) => Bordered (Vector e) Int e
  where
    lower   _ = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)
    
    sizeOf = V.length

--------------------------------------------------------------------------------

{- Indexed, IFold and Sort instances. -}

instance (Unboxed e, Unbox e) => Indexed (Vector e) Int e
  where
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es = defaultBounds (sizeOf es) `assoc` ies
      where
        ies = [ (offsetOf es i, e) | (i, e) <- assocs es, indexIn es i ]
    
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
    (*$) = \ p -> listL . V.findIndices p

instance (Unboxed e, Unbox e) => IFold (Vector e) Int e
  where
    ifoldr = V.ifoldr
    ifoldl = \ f -> V.ifoldl (flip f)
    
    i_foldl = V.foldl
    i_foldr = V.foldr

instance (Unboxed e, Unbox e) => Sort (Vector e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance (Unboxed e, Unbox e) => Thaw (ST s) (Vector e) (STBytes    s Int e) where thaw = fromIndexed'
instance (Unboxed e, Unbox e) => Thaw (ST s) (Vector e) (STUblist   s     e) where thaw = fromIndexed'
instance (Unboxed e, Unbox e) => Thaw (ST s) (Vector e) (STByteList s Int e) where thaw = fromIndexed'

instance (Unboxed e, Unbox e) => Thaw IO (Vector e) (IOBytes    Int e) where thaw = fromIndexed'
instance (Unboxed e, Unbox e) => Thaw IO (Vector e) (IOUblist       e) where thaw = fromIndexed'
instance (Unboxed e, Unbox e) => Thaw IO (Vector e) (IOByteList Int e) where thaw = fromIndexed'



instance (Unboxed e, Unbox e) => Freeze (ST s) (STBytes    s Int e) (Vector e) where freeze = fmap fromList . getLeft
instance (Unboxed e, Unbox e) => Freeze (ST s) (STUblist   s     e) (Vector e) where freeze = fmap fromList . getLeft
instance (Unboxed e, Unbox e) => Freeze (ST s) (STByteList s Int e) (Vector e) where freeze = fmap fromList . getLeft

instance (Unboxed e, Unbox e) => Freeze IO (IOBytes    Int e) (Vector e) where freeze = fmap fromList . getLeft
instance (Unboxed e, Unbox e) => Freeze IO (IOUblist       e) (Vector e) where freeze = fmap fromList . getLeft
instance (Unboxed e, Unbox e) => Freeze IO (IOByteList Int e) (Vector e) where freeze = fmap fromList . getLeft

--------------------------------------------------------------------------------

done :: (Unboxed e, Unbox e) => STBytes s Int e -> ST s (Vector e)
done =  freeze


