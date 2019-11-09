-- Instance of class Foldable for Either ( a)
module InstFoldableEithera where
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

foldMap _ (Left _) = mempty
foldMap f (Right y) = f y

foldr _ z (Left _) = z
foldr f z (Right y) = f y z

length (Left _)  = 0
length (Right _) = 1

null             = isLeft

-- | @since 4.7.0.0
