-- Instance of class Eq for Product (f g a)
module InstEqProductfga where
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
(==) :: (Eq1 f, Eq1 g, Eq a) => Product f g a -> Product f g a -> Bool
(==) = eq1

-- | @since 4.9.0.0
