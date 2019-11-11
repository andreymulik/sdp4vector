{- |
    Module      :  SDP.Vector.Related
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Vector.Related@ is service module that contains 'Default' and
    'Arbitrary' instances for boxed 'Vector' (this implementations do not
    require installation of additional libraries because SDP already depends on
    data-default and QuickCheck).
    
    If you use vectors with other structures, use quickcheck-instances and
    data-default-instances-vector.
-}
module SDP.Vector.Related ( module SDP.Vector ) where

import Prelude ()
import SDP.SafePrelude

import SDP.Vector

import Test.QuickCheck

import Data.Default

default ()

--------------------------------------------------------------------------------

instance Default   (Vector e) where def = Z

instance (Arbitrary e) => Arbitrary (Vector e) where arbitrary = fromList <$> arbitrary


