-- Instance of class Alternative for Kleisli (m a)
module InstAlternativeKleislima where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)

import Control.Applicative

import Control.Arrow ( Kleisli(..) )

empty :: Alternative a => Kleisli a b c
empty = Kleisli $ const Control.Applicative.empty
{-# INLINE empty #-}
Kleisli f <|> Kleisli g = Kleisli $ \x -> f x (Control.Applicative.<|>) g x
{-# INLINE (<|>) #-}

-- | @since 4.14.0.0
