-- Instance of class MonadZip for Product ()
module InstMonadZipProduct where
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
import Data.Semigroup ( Product(..) )
import Control.Monad ( Monad(..) )

mzipWith :: (a -> b -> c) -> Data.Semigroup.Product a -> Data.Semigroup.Product b -> Data.Semigroup.Product c
mzipWith = liftM2

-- | @since 4.8.0.0
