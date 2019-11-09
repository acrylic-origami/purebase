-- Instance of class Foldable for Product (f g)
module InstFoldableProductfg where
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

foldMap f (Pair x y) = Prelude.foldMap f x `mappend` Prelude.foldMap f y

-- | @since 4.9.0.0
