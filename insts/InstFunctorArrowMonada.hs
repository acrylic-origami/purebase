-- Instance of class Functor for ArrowMonad (a)
module InstFunctorArrowMonada where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowMonad(..), arr )

fmap f (ArrowMonad m) = ArrowMonad $ m >>> arr f

-- | @since 4.6.0.0
