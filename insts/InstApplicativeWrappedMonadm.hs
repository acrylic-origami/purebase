-- Instance of class Applicative for WrappedMonad (m)
module InstApplicativeWrappedMonadm where
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


import Control.Applicative ( WrappedMonad(..) )

pure :: Applicative m => a -> WrappedMonad m a
pure = WrapMonad . GHC.Base.pure
WrapMonad f <*> WrapMonad v = WrapMonad (f `ap` v)
liftA2 f (WrapMonad x) (WrapMonad y) = WrapMonad (liftM2 f x y)

-- | @since 2.01
