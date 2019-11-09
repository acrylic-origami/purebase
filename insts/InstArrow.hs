-- Instance of class Arrow for  (->)
module InstArrow.hs where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( Arrow(..) )

    arr f = f
--  (f *** g) ~(x,y) = (f x, g y)
--  sorry, although the above defn is fully H'98, nhc98 can't parse it.
    (***) f g ~(x,y) = (f x, g y)

-- | Kleisli arrows of a monad.
