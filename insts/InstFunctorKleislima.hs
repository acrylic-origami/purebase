-- Instance of class Functor for Kleisli (m a)
module InstFunctorKleislima where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( Kleisli(..) )

fmap f (Kleisli g) = Kleisli (Prelude.fmap f Control.Category.. g)

-- | @since 4.14.0.0
