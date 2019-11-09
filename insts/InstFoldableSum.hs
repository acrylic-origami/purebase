-- Instance of class Foldable for Sum ()
module InstFoldableSum where
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

foldMap :: Monoid m => (a -> m) -> Sum a -> m
foldMap            = coerce

elem :: Eq a => a -> Sum a -> Bool
elem               = (. getSum) #. (==)
foldl :: (b -> a -> b) -> b -> Sum a -> b
foldl              = coerce
foldl' :: (b -> a -> b) -> b -> Sum a -> b
foldl'             = coerce
foldl1 _           = getSum
foldr f z (Sum x) = f x z
foldr' :: (a -> b -> b) -> b -> Sum a -> b
foldr'             = InstFoldableSum.foldr
foldr1 _           = getSum
length _           = 1
maximum            = getSum
minimum            = getSum
null _             = False
product            = getSum
sum                = getSum
toList (Sum x)    = [x]

-- | @since 4.8.0.0
