-- Instance of class Read for Product (f g a)
module InstReadProductfga where
import Control.Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(mzipWith))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted product of functors.

import Data.Functor.Product ( Product(..) )

readPrec = readPrec1

readListPrec = readListPrecDefault
readList     = readListDefault

-- | @since 4.9.0.0
