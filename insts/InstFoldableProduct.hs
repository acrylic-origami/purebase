-- Instance of class Foldable for Product ()
module InstFoldableProduct where
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

foldMap :: Monoid m => (a -> m) -> Product a -> m
foldMap            = coerce

elem :: Eq a => a -> Product a -> Bool
elem               = (. getProduct) #. (==)
foldl :: (b -> a -> b) -> b -> Product a -> b
foldl              = coerce
foldl' :: (b -> a -> b) -> b -> Product a -> b
foldl'             = coerce
foldl1 _           = getProduct
foldr f z (Product x) = f x z
foldr' :: (a -> b -> b) -> b -> Product a -> b
foldr'             = InstFoldableProduct.foldr
foldr1 _           = getProduct
length _           = 1
maximum            = getProduct
minimum            = getProduct
null _             = False
product            = getProduct
sum                = getProduct
toList (Product x)    = [x]

-- | @since 4.8.0.0
