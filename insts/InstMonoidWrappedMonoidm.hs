-- Instance of class Monoid for WrappedMonoid (m)
module InstMonoidWrappedMonoidm where
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

import Data.Semigroup ( WrappedMonoid(..) )

mempty :: Monoid a => WrappedMonoid a
mempty = WrapMonoid Prelude.mempty

-- | @since 4.9.0.0
