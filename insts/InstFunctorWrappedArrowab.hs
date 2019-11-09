-- Instance of class Functor for WrappedArrow (a b)
module InstFunctorWrappedArrowab where
import Control.Category hiding ((.), id)
import Control.Arrow
import Data.Maybe
import Data.Tuple
import Data.Eq
import Data.Ord
import Data.Foldable (Foldable(..))
import Data.Functor ((<$>))
import Data.Functor.Const (Const(..))

import GHC.Base
import GHC.Generics
import GHC.List (repeat, zipWith, drop)
import GHC.Read (Read)
import GHC.Show (Show)


import Control.Applicative ( WrappedArrow(..) )

fmap f (WrapArrow a) = WrapArrow (a >>> arr f)

-- | @since 2.01
