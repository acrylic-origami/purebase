-- Instance of class Foldable for Proxy ()
module InstFoldableProxy where
import Data.Bool
import Data.Either
import Data.Eq
import Data.Functor.Utils (Max(..), Min(..), (#.))
import qualified GHC.List as List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Proxy

import GHC.Arr  ( Array(..), elems, numElements,
  foldlElems, foldrElems,
  foldlElems', foldrElems',
  foldl1Elems, foldr1Elems)
import GHC.Base hiding ( foldr )
import GHC.Generics
import GHC.Num  ( Num(..) )


import Data.Foldable ( Foldable(..) )

foldMap _ _ = mempty
{-# INLINE foldMap #-}
fold _ = mempty
{-# INLINE fold #-}
foldr _ z _ = z
{-# INLINE foldr #-}
foldl _ z _ = z
{-# INLINE foldl #-}
foldl1 _ _ = errorWithoutStackTrace "foldl1: Proxy"
foldr1 _ _ = errorWithoutStackTrace "foldr1: Proxy"
length _   = 0
null _     = True
elem _ _   = False
sum _      = 0
product _  = 1

-- | @since 4.8.0.0
