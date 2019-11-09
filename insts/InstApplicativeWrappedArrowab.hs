-- Instance of class Applicative for WrappedArrow (a b)
module InstApplicativeWrappedArrowab where
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

pure x = WrapArrow (arr (const x))
liftA2 f (WrapArrow u) (WrapArrow v) =
  WrapArrow (u &&& v >>> arr (uncurry f))

-- | @since 2.01
