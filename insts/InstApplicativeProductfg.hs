-- Instance of class Applicative for Product (f g)
module InstApplicativeProductfg where
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

pure x = Pair (Control.Applicative.pure x) (Control.Applicative.pure x)
Pair f g <*> Pair x y = Pair (f Control.Applicative.<*> x) (g Control.Applicative.<*> y)
liftA2 f (Pair a b) (Pair x y) = Pair (Control.Applicative.liftA2 f a x) (Control.Applicative.liftA2 f b y)

-- | @since 4.9.0.0
