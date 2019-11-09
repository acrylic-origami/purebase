-- Instance of class ArrowZero for Kleisli (m)
module InstArrowZeroKleislim where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowZero(..), Kleisli(..) )

zeroArrow :: MonadPlus m => Kleisli m a b
zeroArrow = Kleisli (\_ -> mzero)

-- | A monoid on arrows.
