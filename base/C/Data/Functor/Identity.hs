{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Identity
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The identity functor and monad.
--
-- This trivial type constructor serves two purposes:
--
-- * It can be used with functions parameterized by functor or monad classes.
--
-- * It can be used as a base monad to which a series of monad
--   transformers may be applied to construct a composite monad.
--   Most monad transformer modules include the special case of
--   applying the transformer to 'Identity'.  For example, @State s@
--   is an abbreviation for @StateT s 'Identity'@.
--
-- @since 4.8.0.0
-----------------------------------------------------------------------------

module C.Data.Functor.Identity (
    Identity(..),
  ) where

import Control.Monad.Fix
import Data.Bits (Bits, FiniteBits)
import Data.Coerce
import Data.Foldable
import Data.Functor.Utils ((#.))
import Foreign.Storable (Storable)
import GHC.Arr (Ix)
import GHC.Base ( Applicative(..), Eq(..), Functor(..), Monad(..)
                , Semigroup, Monoid, Ord(..), ($), (.) )
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Generics (Generic, Generic1)
import GHC.Num (Num)
import GHC.Read (Read(..), lex, readParen)
import GHC.Real (Fractional, Integral, Real, RealFrac)
import GHC.Show (Show(..), showParen, showString)
import GHC.Types (Bool(..))

-- | Identity functor and monad. (a non-strict monad)
--
-- @since 4.8.0.0
import Data.Functor.Identity ( Identity(..) )

