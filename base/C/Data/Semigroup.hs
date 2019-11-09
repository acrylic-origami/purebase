{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TypeOperators              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type @a@ is a 'Semigroup' if it provides an associative function ('<>')
-- that lets you combine any two values of type @a@ into one. Where being
-- associative means that the following must always hold:
--
-- >>> (a <> b) <> c == a <> (b <> c)
--
-- ==== __Examples__
--
-- The 'Min' 'Semigroup' instance for 'Int' is defined to always pick the smaller
-- number:
-- >>> Min 1 <> Min 2 <> Min 3 <> Min 4 :: Min Int
-- Min {getMin = 1}
--
-- If we need to combine multiple values we can use the 'sconcat' function
-- to do so. We need to ensure however that we have at least one value to
-- operate on, since otherwise our result would be undefined. It is for this
-- reason that 'sconcat' uses "Data.List.NonEmpty.NonEmpty" - a list that
-- can never be empty:
--
-- >>> (1 :| [])
-- 1 :| []               -- equivalent to [1] but guaranteed to be non-empty
-- >>> (1 :| [2, 3, 4])
-- 1 :| [2,3,4]          -- equivalent to [1,2,3,4] but guaranteed to be non-empty
--
-- Equipped with this guaranteed to be non-empty data structure, we can combine
-- values using 'sconcat' and a 'Semigroup' of our choosing. We can try the 'Min'
-- and 'Max' instances of 'Int' which pick the smallest, or largest number
-- respectively:
--
-- >>> sconcat (1 :| [2, 3, 4]) :: Min Int
-- Min {getMin = 1}
-- >>> sconcat (1 :| [2, 3, 4]) :: Max Int
-- Max {getMax = 4}
--
-- String concatenation is another example of a 'Semigroup' instance:
--
-- >>> "foo" <> "bar"
-- "foobar"
--
-- A 'Semigroup' is a generalization of a 'Monoid'. Yet unlike the 'Semigroup', the 'Monoid'
-- requires the presence of a neutral element ('mempty') in addition to the associative
-- operator. The requirement for a neutral element prevents many types from being a full Monoid,
-- like "Data.List.NonEmpty.NonEmpty".
--
-- Note that the use of @(\<\>)@ in this module conflicts with an operator with the same
-- name that is being exported by "Data.Monoid". However, this package
-- re-exports (most of) the contents of Data.Monoid, so to use semigroups
-- and monoids in the same package just
--
-- > import Data.Semigroup
--
-- @since 4.9.0.0
----------------------------------------------------------------------------
module C.Data.Semigroup (
    Semigroup(..)
  , stimesMonoid
  , stimesIdempotent
  , stimesIdempotentMonoid
  , mtimesDefault
  -- * Semigroups
  , Min(..)
  , Max(..)
  , First(..)
  , Last(..)
  , WrappedMonoid(..)
  -- * Re-exported monoids from Data.Monoid
  , Dual(..)
  , Endo(..)
  , All(..)
  , Any(..)
  , Sum(..)
  , Product(..)
  -- * A better monoid for Maybe
  , Option(..)
  , option
  -- * Difference lists of a semigroup
  , diff
  , cycle1
  -- * ArgMin, ArgMax
  , Arg(..)
  , ArgMin
  , ArgMax
  ) where

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
import Data.Semigroup ( Min(..), Last(..), Max(..), Option(..), WrappedMonoid(..), First(..), Arg(..) )

cycle1 :: Semigroup m => m -> m
cycle1 xs = xs' where xs' = xs <> xs'

-- | This lets you use a difference list of a 'Semigroup' as a 'Monoid'.
diff :: Semigroup m => m -> Endo m
diff = Endo . (<>)

type ArgMin a b = Min (Arg a b)
type ArgMax a b = Max (Arg a b)

-- | @since 4.9.0.0
mtimesDefault :: (Integral b, Monoid a) => b -> a -> a
mtimesDefault n x
  | n == 0    = mempty
  | otherwise = unwrapMonoid (stimes n (WrapMonoid x))

-- | 'Option' is effectively 'Maybe' with a better instance of
-- 'Monoid', built off of an underlying 'Semigroup' instead of an
-- underlying 'Monoid'.
--
-- Ideally, this type would not exist at all and we would just fix the
-- 'Monoid' instance of 'Maybe'.
--
-- In GHC 8.4 and higher, the 'Monoid' instance for 'Maybe' has been
-- corrected to lift a 'Semigroup' instance instead of a 'Monoid'
-- instance. Consequently, this type is no longer useful. It will be
-- marked deprecated in GHC 8.8 and removed in GHC 8.10.
-- | @since 4.9.0.0
option :: b -> (a -> b) -> Option a -> b
option n j (Option m) = maybe n j m

-- | @since 4.9.0.0
