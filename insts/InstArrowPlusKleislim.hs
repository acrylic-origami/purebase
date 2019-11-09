-- Instance of class ArrowPlus for Kleisli (m)
module InstArrowPlusKleislim where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowPlus(..), Kleisli(..) )

Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus` g x)

-- | Choice, for arrows that support it.  This class underlies the
-- @if@ and @case@ constructs in arrow notation.
--
-- Instances should satisfy the following laws:
--
--  * @'left' ('arr' f) = 'arr' ('left' f)@
--
--  * @'left' (f >>> g) = 'left' f >>> 'left' g@
--
--  * @f >>> 'arr' 'Left' = 'arr' 'Left' >>> 'left' f@
--
--  * @'left' f >>> 'arr' ('id' +++ g) = 'arr' ('id' +++ g) >>> 'left' f@
--
--  * @'left' ('left' f) >>> 'arr' assocsum = 'arr' assocsum >>> 'left' f@
--
-- where
--
-- > assocsum (Left (Left x)) = Left x
-- > assocsum (Left (Right y)) = Right (Left y)
-- > assocsum (Right z) = Right (Right z)
--
-- The other combinators have sensible default definitions, which may
-- be overridden for efficiency.

