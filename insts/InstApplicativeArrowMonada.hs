-- Instance of class Applicative for ArrowMonad (a)
module InstApplicativeArrowMonada.hs where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowMonad(..) )

   pure x = ArrowMonad (arr (const x))
   ArrowMonad f <*> ArrowMonad x = ArrowMonad (f &&& x >>> arr (uncurry id))

-- | @since 2.01
