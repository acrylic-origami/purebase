-- Instance of class Applicative for ZipList ()
module InstApplicativeZipList.hs where
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


import Control.Applicative ( ZipList(..) )

    pure x = ZipList (repeat x)
    liftA2 f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

-- | @since 4.11.0.0
