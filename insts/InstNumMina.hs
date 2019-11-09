-- Instance of class Num for Min (a)
module InstNumMina.hs where
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

  (Min a) + (Min b) = Min (a + b)
  (Min a) * (Min b) = Min (a * b)
  (Min a) - (Min b) = Min (a - b)
  negate (Min a) = Min (negate a)
  abs    (Min a) = Min (abs a)
  signum (Min a) = Min (signum a)
  fromInteger    = Min . fromInteger

