-- Instance of class Alternative for ZipList ()
module InstAlternativeZipList where
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

empty = ZipList []
ZipList xs <|> ZipList ys = ZipList (xs ++ drop (length xs) ys)

-- extra functions

-- | One or none.
