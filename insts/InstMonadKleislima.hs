-- Instance of class Monad for Kleisli (m a)
module InstMonadKleislima where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( Kleisli(..) )

Kleisli f >>= k = Kleisli $ \x -> f x GHC.Base.>>= \a -> runKleisli (k a) x
{-# INLINE (>>=) #-}

-- | @since 4.14.0.0
