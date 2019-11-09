-- Instance of class Functor for Product (f g)
module InstFunctorProductfg.hs where
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

    fmap f (Pair x y) = Pair (fmap f x) (fmap f y)
    a <$ (Pair x y) = Pair (a <$ x) (a <$ y)

-- | @since 4.9.0.0
