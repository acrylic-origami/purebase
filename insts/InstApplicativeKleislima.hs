-- Instance of class Applicative for Kleisli (m a)
module InstApplicativeKleislima where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( Kleisli(..) )

-- pure :: Applicative m => Kleisli m a a
-- pure = Kleisli Control.Category.. const Control.Category.. GHC.Base.pure
-- {-# INLINE pure #-} -- TODO
Kleisli f <*> Kleisli g = Kleisli $ \x -> f x GHC.Base.<*> g x
{-# INLINE (<*>) #-}
Kleisli f *> Kleisli g = Kleisli $ \x -> f x GHC.Base.*> g x
{-# INLINE (*>) #-}
Kleisli f <* Kleisli g = Kleisli $ \x -> f x GHC.Base.<* g x
{-# INLINE (<*) #-}

-- | @since 4.14.0.0
