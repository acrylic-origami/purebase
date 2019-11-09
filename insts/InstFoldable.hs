-- Instance of class Foldable for  ([])
module InstFoldable where
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

elem :: Eq a => a -> [a] -> Bool
elem    = List.elem
foldl   = List.foldl
foldl'  = List.foldl'
foldl1  = List.foldl1
foldr   = List.foldr
foldr1  = List.foldr1
length  = List.length
maximum :: Ord a => [a] -> a
maximum = List.maximum
minimum :: Ord a => [a] -> a
minimum = List.minimum
null    = List.null
product = List.product
sum     = List.sum
toList  = id

-- | @since 4.9.0.0
