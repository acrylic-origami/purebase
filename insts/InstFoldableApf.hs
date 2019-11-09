-- Instance of class Foldable for Ap (f)
module InstFoldableApf.hs where
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

    foldMap f = foldMap f . getAp

-- Instances for GHC.Generics
-- | @since 4.9.0.0
