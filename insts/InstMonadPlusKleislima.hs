-- Instance of class MonadPlus for Kleisli (m a)
module InstMonadPlusKleislima.hs where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( Kleisli(..) )

  mzero = Kleisli $ const mzero
  {-# INLINE mzero #-}
  Kleisli f `mplus` Kleisli g = Kleisli $ \x -> f x `mplus` g x
  {-# INLINE mplus #-}

-- | @since 3.0
