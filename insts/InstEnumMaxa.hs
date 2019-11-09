-- Instance of class Enum for Max (a)
module InstEnumMaxa where
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

import Data.Semigroup ( Max(..) )

-- succ (Max a) = Max (succ a)
-- pred (Max a) = Max (pred a)
-- toEnum = Max . toEnum
-- fromEnum = fromEnum . getMax
-- enumFrom (Max a) = Max <$> enumFrom a
-- enumFromThen (Max a) (Max b) = Max <$> enumFromThen a b
-- enumFromTo (Max a) (Max b) = Max <$> enumFromTo a b
-- enumFromThenTo (Max a) (Max b) (Max c) = Max <$> enumFromThenTo a b c
-- TODO

-- | @since 4.9.0.0
