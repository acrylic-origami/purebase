-- Instance of class Read for Sum (f g a)
module InstReadSumfga where
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.

import Data.Functor.Sum ( Sum(..) )

readPrec = readPrec1

readListPrec = readListPrecDefault
readList     = readListDefault
-- | @since 4.9.0.0
