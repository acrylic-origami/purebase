-- Instance of class Functor for Arg ( a)
module InstFunctorArga where
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

import Data.Semigroup ( Arg(..) )

fmap f (Arg x a) = Arg x (f a)

-- | @since 4.9.0.0
