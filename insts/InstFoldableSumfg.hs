-- Instance of class Foldable for Sum (f g)
module InstFoldableSumfg.hs where
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.

import Data.Functor.Sum ( Sum(..) )

    foldMap f (InL x) = foldMap f x
    foldMap f (InR y) = foldMap f y

-- | @since 4.9.0.0
