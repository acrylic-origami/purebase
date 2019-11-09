-- Instance of class MonadZip for First ()
module InstMonadZipFirst.hs where
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

    mzipWith = liftM2

-- | @since 4.8.0.0
