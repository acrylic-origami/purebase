-- Instance of class Functor for Product (f g)
module InstFunctorProductfg where
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

fmap f (Pair x y) = Pair (Prelude.fmap f x) (Prelude.fmap f y)
a <$ (Pair x y) = Pair (a Control.Applicative.<$ x) (a Control.Applicative.<$ y)

-- | @since 4.9.0.0
