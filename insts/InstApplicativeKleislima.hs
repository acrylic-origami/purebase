-- Instance of class Applicative for Kleisli (m a)
module InstApplicativeKleislima.hs where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( Kleisli(..) )

  pure = Kleisli . const . pure
  {-# INLINE pure #-}
  Kleisli f <*> Kleisli g = Kleisli $ \x -> f x <*> g x
  {-# INLINE (<*>) #-}
  Kleisli f *> Kleisli g = Kleisli $ \x -> f x *> g x
  {-# INLINE (*>) #-}
  Kleisli f <* Kleisli g = Kleisli $ \x -> f x <* g x
  {-# INLINE (<*) #-}

-- | @since 4.14.0.0
