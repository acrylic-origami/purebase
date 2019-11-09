-- Instance of class MonadPlus for Kleisli (m a)
module InstMonadPlusKleislima where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( Kleisli(..) )

mzero :: MonadPlus m => Kleisli m a b
mzero = Kleisli $ const GHC.Base.mzero
{-# INLINE mzero #-}
Kleisli f `mplus` Kleisli g = Kleisli $ \x -> f x `GHC.Base.mplus` g x
{-# INLINE mplus #-}

-- | @since 3.0
