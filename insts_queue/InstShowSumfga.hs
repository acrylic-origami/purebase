-- Instance of class Show for Sum (f g a)
module InstShowSumfga where
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.

import Data.Functor.Sum ( Sum(..) )

showsPrec = showsPrec1

-- | @since 4.9.0.0
