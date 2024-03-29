-- Instance of class Foldable for Dual ()
module InstFoldableDual where
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

foldMap :: Monoid m => (a -> m) -> Dual a -> m
foldMap            = coerce

elem :: Eq a => a -> Dual a -> Bool
elem               = (. getDual) #. (==)
foldl :: (b -> a -> b) -> b -> Dual a -> b
foldl              = coerce
foldl' :: (b -> a -> b) -> b -> Dual a -> b
foldl'             = coerce
foldl1 _           = getDual
foldr f z (Dual x) = f x z
foldr' :: (a -> b -> b) -> b -> Dual a -> b
foldr'             = InstFoldableDual.foldr
foldr1 _           = getDual
length _           = 1
maximum            = getDual
minimum            = getDual
null _             = False
product            = getDual
sum                = getDual
toList (Dual x)    = [x]

-- | @since 4.8.0.0
