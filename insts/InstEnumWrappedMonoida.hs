-- Instance of class Enum for WrappedMonoid (a)
module InstEnumWrappedMonoida where
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

-- succ (WrapMonoid a) = WrapMonoid (succ a)
-- pred (WrapMonoid a) = WrapMonoid (pred a)
-- toEnum = WrapMonoid . toEnum
-- fromEnum = fromEnum . unwrapMonoid
-- enumFrom (WrapMonoid a) = WrapMonoid <$> enumFrom a
-- enumFromThen (WrapMonoid a) (WrapMonoid b) = WrapMonoid <$> enumFromThen a b
-- enumFromTo (WrapMonoid a) (WrapMonoid b) = WrapMonoid <$> enumFromTo a b
-- enumFromThenTo (WrapMonoid a) (WrapMonoid b) (WrapMonoid c) =
--   WrapMonoid <$> enumFromThenTo a b c
-- TODO

-- | Repeat a value @n@ times.
--
-- > mtimesDefault n a = a <> a <> ... <> a  -- using <> (n-1) times
--
-- Implemented using 'stimes' and 'mempty'.
--
-- This is a suitable definition for an 'mtimes' member of 'Monoid'.
