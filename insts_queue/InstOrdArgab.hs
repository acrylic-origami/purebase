-- Instance of class Ord for Arg (a b)
module InstOrdArgab where
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

Arg a _ `compare` Arg b _ = compare a b
min x@(Arg a _) y@(Arg b _)
    | a <= b    = x
    | otherwise = y
max x@(Arg a _) y@(Arg b _)
    | a >= b    = x
    | otherwise = y

-- | @since 4.9.0.0
