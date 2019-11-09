-- Instance of class MonadZip for M1 (i c f)
module InstMonadZipM1icf where
import Control.Monad (liftM, liftM2)
import Data.Functor.Identity
import Data.Monoid
import Data.Ord ( Down(..) )
import Data.Proxy
import qualified Data.List.NonEmpty as NE
import GHC.Generics

-- | Instances should satisfy the laws:
--
-- [Naturality]
--
--     @'liftM' (f 'Control.Arrow.***' g) ('mzip' ma mb)
--         = 'mzip' ('liftM' f ma) ('liftM' g mb)@
--
-- [Information Preservation]
--
--     @'liftM' ('Prelude.const' ()) ma = 'liftM' ('Prelude.const' ()) mb@
--         implies
--     @'munzip' ('mzip' ma mb) = (ma, mb)@
--

import Control.Monad.Zip ( MonadZip(..) )
-- import Control.Monad ( Monad(..) )

mzipWith :: MonadZip m => (a -> b -> c) -> M1 k1 l1 m a -> M1 k2 l2 m b -> M1 k3 l3 m c
mzipWith f (M1 fa) (M1 fb) = M1 (Control.Monad.Zip.mzipWith f fa fb)

-- | @since 4.9.0.0
