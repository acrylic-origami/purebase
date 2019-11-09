-- Instance of class Category for Kleisli (m)
module InstCategoryKleislim.hs where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( Kleisli(..) )

    id = Kleisli return
    (Kleisli f) . (Kleisli g) = Kleisli (\b -> g b >>= f)

-- | @since 2.01
