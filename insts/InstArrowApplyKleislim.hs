-- Instance of class ArrowApply for Kleisli (m)
module InstArrowApplyKleislim where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowApply(..), Kleisli(..) )

app = Kleisli (\(Kleisli f, x) -> f x)

-- | The 'ArrowApply' class is equivalent to 'Monad': any monad gives rise
--   to a 'Kleisli' arrow, and any instance of 'ArrowApply' defines a monad.

