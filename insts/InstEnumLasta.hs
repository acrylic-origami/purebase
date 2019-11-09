-- Instance of class Enum for Last (a)
module InstEnumLasta where
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

import Data.Semigroup ( Last(..) )

-- succ (Last a) = Last (succ a)
-- pred (Last a) = Last (pred a)
-- toEnum = Last . toEnum
-- fromEnum = fromEnum . getLast
-- enumFrom (Last a) = Last <$> enumFrom a
-- enumFromThen (Last a) (Last b) = Last <$> enumFromThen a b
-- enumFromTo (Last a) (Last b) = Last <$> enumFromTo a b
-- enumFromThenTo (Last a) (Last b) (Last c) = Last <$> enumFromThenTo a b c
-- TODO

-- | @since 4.9.0.0
