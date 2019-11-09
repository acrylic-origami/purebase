-- Instance of class Foldable for Array ( i)
module InstFoldableArrayi.hs where
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

    foldr = foldrElems
    foldl = foldlElems
    foldl' = foldlElems'
    foldr' = foldrElems'
    foldl1 = foldl1Elems
    foldr1 = foldr1Elems
    toList = elems
    length = numElements
    null a = numElements a == 0

-- | @since 4.7.0.0
