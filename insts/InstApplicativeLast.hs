-- Instance of class Applicative for Last ()
module InstApplicativeLast where
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

pure = Last
a <* _ = a
_ *> a = a
(<*>) :: Last (a -> b) -> Last a -> Last b
(<*>) = coerce
liftA2 :: (a -> b -> c) -> Last a -> Last b -> Last c
liftA2 = coerce

-- | @since 4.9.0.0
