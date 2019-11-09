-- Instance of class Bitraversable for Arg ()
module InstBitraversableArg.hs where
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

  bitraverse f g (Arg a b) = Arg <$> f a <*> g b

-- | Use @'Option' ('First' a)@ to get the behavior of
-- 'Data.Monoid.First' from "Data.Monoid".
