-- Instance of class MonadZip for f (:*: g)
module InstMonadZipfg where
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

mzipWith :: (MonadZip f, MonadZip g) => (a -> b -> c) -> (:*:) f g a -> (:*:) f g b -> (:*:) f g c
mzipWith f (x1 :*: y1) (x2 :*: y2) = Control.Monad.Zip.mzipWith f x1 x2 :*: Control.Monad.Zip.mzipWith f y1 y2

-- instances for Data.Ord

-- | @since 4.12.0.0
