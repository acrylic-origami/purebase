-- Instance of class Monad for Product (f g)
module InstMonadProductfg where
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

Pair m n >>= f = Pair (m Prelude.>>= fstP . f) (n Prelude.>>= sndP . f)
  where
    fstP (Pair a _) = a
    sndP (Pair _ b) = b

-- | @since 4.9.0.0
