-- Instance of class Alternative for WrappedMonad (m)
module InstAlternativeWrappedMonadm.hs where
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

    empty = WrapMonad mzero
    WrapMonad u <|> WrapMonad v = WrapMonad (u `mplus` v)

