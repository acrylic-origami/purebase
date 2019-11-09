-- Instance of class Enum for First (a)
module InstEnumFirsta where
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

succ (First a) = First (Prelude.succ a)
pred (First a) = First (Prelude.pred a)
toEnum :: Enum a => Int -> First a
toEnum = First . Prelude.toEnum
fromEnum :: Enum a => First a -> Int
fromEnum = Prelude.fromEnum . getFirst
enumFrom (First a) = First <$> Prelude.enumFrom a
enumFromThen (First a) (First b) = First <$> Prelude.enumFromThen a b
enumFromTo (First a) (First b) = First <$> Prelude.enumFromTo a b
enumFromThenTo (First a) (First b) (First c) = First <$> Prelude.enumFromThenTo a b c

-- | @since 4.9.0.0
