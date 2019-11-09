-- Instance of class Traversable for Sum (f g)
module InstTraversableSumfg.hs where
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.

import Data.Functor.Sum ( Sum(..) )

    traverse f (InL x) = InL <$> traverse f x
    traverse f (InR y) = InR <$> traverse f y
