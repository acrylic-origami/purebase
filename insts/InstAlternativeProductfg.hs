-- Instance of class Alternative for Product (f g)
module InstAlternativeProductfg where
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

empty :: (Alternative f, Alternative g) => Product f g a
empty = Pair Control.Applicative.empty Control.Applicative.empty
Pair x1 y1 <|> Pair x2 y2 = Pair (x1 Control.Applicative.<|> x2) (y1 Control.Applicative.<|> y2)

-- | @since 4.9.0.0
