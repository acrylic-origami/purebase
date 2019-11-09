-- Instance of class MonadFix for Product (f g)
module InstMonadFixProductfg.hs where
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

    mfix f = Pair (mfix (fstP . f)) (mfix (sndP . f))
      where
        fstP (Pair a _) = a
        sndP (Pair _ b) = b

-- | @since 4.9.0.0
