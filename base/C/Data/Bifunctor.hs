{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor
-- Copyright   :  (C) 2008-2014 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- @since 4.8.0.0
----------------------------------------------------------------------------
module C.Data.Bifunctor
  ( Bifunctor(..)
  ) where

import Control.Applicative  ( Const(..) )
import GHC.Generics ( K1(..) )

-- | A bifunctor is a type constructor that takes
-- two type arguments and is a functor in /both/ arguments. That
-- is, unlike with 'Functor', a type constructor such as 'Either'
-- does not need to be partially applied for a 'Bifunctor'
-- instance, and the methods in this class permit mapping
-- functions over the 'Left' value or the 'Right' value,
-- or both at the same time.
--
-- Formally, the class 'Bifunctor' represents a bifunctor
-- from @Hask@ -> @Hask@.
--
-- Intuitively it is a bifunctor where both the first and second
-- arguments are covariant.
--
-- You can define a 'Bifunctor' by either defining 'bimap' or by
-- defining both 'first' and 'second'.
--
-- If you supply 'bimap', you should ensure that:
--
-- @'bimap' 'id' 'id' ≡ 'id'@
--
-- If you supply 'first' and 'second', ensure:
--
-- @
-- 'first' 'id' ≡ 'id'
-- 'second' 'id' ≡ 'id'
-- @
--
-- If you supply both, you should also ensure:
--
-- @'bimap' f g ≡ 'first' f '.' 'second' g@
--
-- These ensure by parametricity:
--
-- @
-- 'bimap'  (f '.' g) (h '.' i) ≡ 'bimap' f h '.' 'bimap' g i
-- 'first'  (f '.' g) ≡ 'first'  f '.' 'first'  g
-- 'second' (f '.' g) ≡ 'second' f '.' 'second' g
-- @
--
-- @since 4.8.0.0    bimap f g ~(x1, a, b) = (x1, f a, g b)

-- | @since 4.8.0.0    bimap f g ~(x1, x2, a, b) = (x1, x2, f a, g b)

-- | @since 4.8.0.0    bimap f g ~(x1, x2, x3, a, b) = (x1, x2, x3, f a, g b)

-- | @since 4.8.0.0    bimap f g ~(x1, x2, x3, x4, a, b) = (x1, x2, x3, x4, f a, g b)

-- | @since 4.8.0.0    bimap f g ~(x1, x2, x3, x4, x5, a, b) = (x1, x2, x3, x4, x5, f a, g b)


-- | @since 4.8.0.0
import Data.Bifunctor ( Bifunctor(..) )

