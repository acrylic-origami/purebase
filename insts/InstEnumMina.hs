-- Instance of class Enum for Min (a)
module InstEnumMina where
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

import Data.Semigroup ( Min(..) )

-- succ (Min a) = Min (succ a)
-- pred (Min a) = Min (pred a)
-- toEnum = Min . toEnum
-- fromEnum = fromEnum . getMin
-- enumFrom (Min a) = Min <$> enumFrom a
-- enumFromThen (Min a) (Min b) = Min <$> enumFromThen a b
-- enumFromTo (Min a) (Min b) = Min <$> enumFromTo a b
-- enumFromThenTo (Min a) (Min b) (Min c) = Min <$> enumFromThenTo a b c 
-- TODO


-- | @since 4.9.0.0
