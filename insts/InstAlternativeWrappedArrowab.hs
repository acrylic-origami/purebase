-- Instance of class Alternative for WrappedArrow (a b)
module InstAlternativeWrappedArrowab where
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

empty :: (ArrowZero a, ArrowPlus a) => WrappedArrow a b z
empty = WrapArrow zeroArrow
WrapArrow u <|> WrapArrow v = WrapArrow (u <+> v)

-- | Lists, but with an 'Applicative' functor based on zipping.
