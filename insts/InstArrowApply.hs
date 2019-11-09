-- Instance of class ArrowApply for  (->)
module InstArrowApply.hs where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowApply(..) )

    app (f,x) = f x

-- | @since 2.01
