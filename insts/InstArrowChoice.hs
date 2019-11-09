-- Instance of class ArrowChoice for  (->)
module InstArrowChoice where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowChoice(..) )

left f = f Control.Arrow.+++ Control.Category.id
right f = Control.Category.id Control.Arrow.+++ f
f +++ g = (Left Control.Category.. f) Control.Arrow.||| (Right Control.Category.. g)
(|||) = either

-- | @since 2.01
