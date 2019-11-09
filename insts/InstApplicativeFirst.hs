-- Instance of class Applicative for First ()
module InstApplicativeFirst where
import           Prelude             hiding (foldr1)

import {-# SOURCE #-} GHC.Base (Semigroup(..))

import           Data.Semigroup.Internal

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Coerce
import           Data.Data
import           GHC.Generics

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.

import Data.Semigroup ( First(..) )

pure x = First x
a <* _ = a
_ *> a = a
(<*>) :: First (a -> b) -> First a -> First b
(<*>) = coerce
liftA2 :: (a -> b -> c) -> First a -> First b -> First c
liftA2 = coerce

-- | @since 4.9.0.0
