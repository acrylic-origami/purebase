{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Contravariant
-- Copyright   :  (C) 2007-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- 'Contravariant' functors, sometimes referred to colloquially as @Cofunctor@,
-- even though the dual of a 'Functor' is just a 'Functor'. As with 'Functor'
-- the definition of 'Contravariant' for a given ADT is unambiguous.
--
-- @since 4.12.0.0
----------------------------------------------------------------------------

module C.Data.Functor.Contravariant (
  -- * Contravariant Functors
    Contravariant(..)
  , phantom

  -- * Operators
  , (>$<), (>$$<), ($<)

  -- * Predicates
  , Predicate(..)

  -- * Comparisons
  , Comparison(..)
  , defaultComparison

  -- * Equivalence Relations
  , Equivalence(..)
  , defaultEquivalence
  , comparisonEquivalence

  -- * Dual arrows
  , Op(..)
  ) where

import Control.Applicative
import Control.Category
import Data.Function (on)

import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

import Data.Monoid (Alt(..))
import Data.Proxy
import GHC.Generics

import Prelude hiding ((.),id)

-- | The class of contravariant functors.
--
-- Whereas in Haskell, one can think of a 'Functor' as containing or producing
-- values, a contravariant functor is a functor that can be thought of as
-- /consuming/ values.
--
-- As an example, consider the type of predicate functions  @a -> Bool@. One
-- such predicate might be @negative x = x < 0@, which
-- classifies integers as to whether they are negative. However, given this
-- predicate, we can re-use it in other situations, providing we have a way to
-- map values /to/ integers. For instance, we can use the @negative@ predicate
-- on a person's bank balance to work out if they are currently overdrawn:
--
-- @
-- newtype Predicate a = Predicate { getPredicate :: a -> Bool }
--
-- instance Contravariant Predicate where
--   contramap f (Predicate p) = Predicate (p . f)
--                                          |   `- First, map the input...
--                                          `----- then apply the predicate.
--
-- overdrawn :: Predicate Person
-- overdrawn = contramap personBankBalance negative
-- @
--
-- Any instance should be subject to the following laws:
--
-- [Identity]    @'contramap' 'id' = 'id'@
-- [Composition] @'contramap' (g . f) = 'contramap' f . 'contramap' g@
--
-- Note, that the second law follows from the free theorem of the type of
-- 'contramap' and the first law, so you need only check that the former
-- condition holds.

import Data.Functor.Contravariant ( Contravariant(..), Predicate(..), Comparison(..), Equivalence(..), Op(..) )

phantom :: (Functor f, Contravariant f) => f a -> f b
phantom x = () <$ x $< ()

infixl 4 $<, >$<, >$$<

-- | This is '>$' with its arguments flipped.
($<) :: Contravariant f => f b -> b -> f a
($<) = flip (>$)

-- | This is an infix alias for 'contramap'.
(>$<) :: Contravariant f => (a -> b) -> f b -> f a
(>$<) = contramap

-- | This is an infix version of 'contramap' with the arguments flipped.
(>$$<) :: Contravariant f => f b -> (a -> b) -> f a
(>$$<) = flip contramap








-- | A 'Comparison' is a 'Contravariant' 'Functor', because 'contramap' can
-- apply its function argument to each input of the comparison function.
defaultComparison :: Ord a => Comparison a
defaultComparison = Comparison compare

-- | This data type represents an equivalence relation.
--
-- Equivalence relations are expected to satisfy three laws:
--
-- [Reflexivity]:  @'getEquivalence' f a a = True@
-- [Symmetry]:     @'getEquivalence' f a b = 'getEquivalence' f b a@
-- [Transitivity]:
--    If @'getEquivalence' f a b@ and @'getEquivalence' f b c@ are both 'True'
--    then so is @'getEquivalence' f a c@.
--
-- The types alone do not enforce these laws, so you'll have to check them
-- yourself.
defaultEquivalence :: Eq a => Equivalence a
defaultEquivalence = Equivalence (==)

comparisonEquivalence :: Comparison a -> Equivalence a
comparisonEquivalence (Comparison p) = Equivalence $ \a b -> p a b == EQ

-- | Dual function arrows.



