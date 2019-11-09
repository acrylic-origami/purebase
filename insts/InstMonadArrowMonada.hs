-- Instance of class Monad for ArrowMonad (a)
module InstMonadArrowMonada where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowMonad(..), arr, app )

ArrowMonad m >>= f = ArrowMonad $
  m >>> arr (\x -> let ArrowMonad h = f x in (h, ())) >>> app

-- | @since 4.6.0.0
