-- Instance of class MonadZip for Alt (f)
module InstMonadZipAltf.hs where
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

    mzipWith f (Alt ma) (Alt mb) = Alt (mzipWith f ma mb)

-- | @since 4.9.0.0
