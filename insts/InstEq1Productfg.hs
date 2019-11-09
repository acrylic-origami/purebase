-- Instance of class Eq1 for Product (f g)
module InstEq1Productfg where
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

liftEq eq (Pair x1 y1) (Pair x2 y2) = Data.Functor.Classes.liftEq eq x1 x2 && Data.Functor.Classes.liftEq eq y1 y2

-- | @since 4.9.0.0
