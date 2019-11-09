-- Instance of class MonadPlus for Product (f g)
module InstMonadPlusProductfg where
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

mzero :: (MonadPlus a, MonadPlus b) => Product a b c
mzero = Pair Control.Monad.mzero Control.Monad.mzero
Pair x1 y1 `mplus` Pair x2 y2 = Pair (x1 `Control.Monad.mplus` x2) (y1 `Control.Monad.mplus` y2)

-- | @since 4.9.0.0
