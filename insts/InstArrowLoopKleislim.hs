-- Instance of class ArrowLoop for Kleisli (m)
module InstArrowLoopKleislim where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowLoop(..), Kleisli(..) )

loop (Kleisli f) = Kleisli (liftM fst Control.Category.. mfix Control.Category.. f')
  where f' x y = f (x, snd y)
