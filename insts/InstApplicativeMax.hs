-- Instance of class Applicative for Max ()
module InstApplicativeMax where
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

pure = Max
a <* _ = a
_ *> a = a

(<*>) :: Max (a -> b) -> Max a -> Max b
(<*>) = coerce
liftA2 :: (a -> b -> c) -> Max a -> Max b -> Max c
liftA2 = coerce

-- | @since 4.9.0.0
