-- Instance of class ArrowLoop for  (->)
module InstArrowLoop where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowLoop(..) )

loop f b = let (c,d) = f (b,d) in c

-- | Beware that for many monads (those for which the '>>=' operation
-- is strict) this instance will /not/ satisfy the right-tightening law
-- required by the 'ArrowLoop' class.
--
-- @since 2.01
