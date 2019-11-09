-- Instance of class MonadZip for Product (f g)
module InstMonadZipProductfg where
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

mzipWith :: (MonadZip f, MonadZip g) => (a -> b -> c) -> Product f g a -> Product f g b -> Product f g c
mzipWith f (Pair x1 y1) (Pair x2 y2) = Pair (Control.Monad.Zip.mzipWith f x1 x2) (Control.Monad.Zip.mzipWith f y1 y2)
