-- Instance of class MonadZip for Rec1 (f)
module InstMonadZipRec1f where
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

mzipWith :: (MonadZip f) => (a -> b -> c) -> Rec1 f a -> Rec1 f b -> Rec1 f c
mzipWith f (Rec1 fa) (Rec1 fb) = Rec1 (Control.Monad.Zip.mzipWith f fa fb)

-- | @since 4.9.0.0
